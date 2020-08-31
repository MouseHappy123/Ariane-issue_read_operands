from pyhcl import *
from functools import reduce

# defined in ariane_pkg.sv

NONE      = 0
LOAD      = 1
STORE     = 2
ALU       = 3
CTRL_FLOW = 4
MULT      = 5
CSR       = 6
FPU       = 7
FPU_VEC   = 8

fu_data_o = {}
fu_q = {}
operator_q = {}
trans_id_q = {}
alu_valid_o = {}
branch_valid_o = {}
lsu_valid_o = {}
csr_valid_o = {}
mult_valid_o = {}
fpu_valid_o = {}
fpu_fmt_o = {}
fpu_rm_o = {}
issue_instr_i = {}
flu_ready_i = {}
fpu_ready_i = {}
lsu_ready_i = {}
rs1_valid_i = {}
rs2_valid_i = {}
rs3_valid_i = {}

def is_rs1_fpr(): ...
def is_rs2_fpr(): ...
def is_imm_fpr(): ...

rd_clobber_fpr_i = []
rd_clobber_gpr_i = []

REG_ADDR_SIZE = 6

def issue_read_operands(NR_COMMIT_PORTS: int):
  class issue_read_operands(Module):
    io = IO(
      clk_i=Input(U.w(4)), # Clock
      rst_ni=Input(U.w(4)), # Asynchronous reset active low
      # flush
      flush_i=Input(U.w(4)),
      # coming from rename
      #input  scoreboard_entry_t                      issue_instr_i,
      issue_instr_valid_i=Input(U.w(4)),
      issue_ack_o=Output(U.w(4)),
      # lookup rd in scoreboard
      #output logic [REG_ADDR_SIZE-1:0]               rs1_o,
      rs1_i=Input(Vec(64,U.w(4))),
      rs1_valid_i=Input(U.w(4)),
      #output logic [REG_ADDR_SIZE-1:0]               rs2_o,
      rs2_i=Input(Vec(64,U.w(4))),
      rs2_valid_i=Input(U.w(4)),
      #output logic [REG_ADDR_SIZE-1:0]               rs3_o,
      #input  logic [FLEN-1:0]                        rs3_i,
      rs3_valid_i=Input(U.w(4)),
      # get clobber input
      #input  fu_t [2**REG_ADDR_SIZE-1:0]             rd_clobber_gpr_i,
      #input  fu_t [2**REG_ADDR_SIZE-1:0]             rd_clobber_fpr_i,
      # To FU, just single issue for now
      #output fu_data_t                               fu_data_o,
      #output logic [riscv::VLEN-1:0]                 pc_o,
      is_compressed_instr_o=Output(U.w(4)),
      # ALU 1
      flu_ready_i=Input(U.w(4)),      # Fixed latency unit ready to accept a new request
      alu_valid_o=Output(U.w(4)),      # Output is valid
      # Branches and Jumps
      branch_valid_o=Output(U.w(4)),   #  this is a valid branch instruction
      #output branchpredict_sbe_t                     branch_predict_o,
      # LSU
      lsu_ready_i=Input(U.w(4)),      # FU is ready
      lsu_valid_o=Output(U.w(4)),      # Output is valid
      # MULT
      mult_valid_o=Output(U.w(4)),     # Output is valid
      # FPU
      fpu_ready_i=Input(U.w(4)),      # FU is ready
      fpu_valid_o=Output(U.w(4)),      # Output is valid
      fpu_fmt_o=Output(Vec(2,U.w(4))),        # FP fmt field from instr.
      fpu_rm_o=Output(Vec(3,U.w(4))),         # FP rm field from instr.
      # CSR
      csr_valid_o=Output(U.w(4)),      # Output is valid
      # commit port
      waddr_i=Input(Vec(NR_COMMIT_PORTS,Vec(5,U.w(4)))),
      wdata_i=Input(Vec(NR_COMMIT_PORTS,Vec(64,U.w(4)))),
      we_gpr_i=Input(Vec(NR_COMMIT_PORTS,U.w(4))),
      we_fpr_i=Input(Vec(NR_COMMIT_PORTS,U.w(4)))
      # committing instruction instruction
      # from scoreboard
      # input  scoreboard_entry     commit_instr_i,
      # output logic                commit_ack_o
    )

    stall=Wire(U.w(4))   # stall signal, we do not want to fetch any more entries
    fu_busy=Wire(U.w(4)) # functional unit is busy
    operand_a_regfile=Wire(Vec(64,U.w(4))) 
    operand_b_regfile=Wire(Vec(64,U.w(4)))   # operands coming from regfile
    # logic [FLEN-1:0] operand_c_regfile; # third operand only from fp regfile
    # output flipflop (ID <-> EX)
    operand_a_n=Wire(Vec(64,U.w(4))) 
    operand_a_q=Wire(Vec(64,U.w(4)))
    operand_b_n=Wire(Vec(64,U.w(4)))
    operand_b_q=Wire(Vec(64,U.w(4)))
    imm_n=Wire(Vec(64,U.w(4)))
    imm_q=Wire(Vec(64,U.w(4)))

    alu_valid_q=Wire(U.w(4))
    mult_valid_q=Wire(U.w(4))
    fpu_valid_q=Wire(U.w(4))
    fpu_fmt_q=Wire(Vec(2,U.w(4)))
    fpu_rm_q=Wire(Vec(3,U.w(4)))
    lsu_valid_q=Wire(U.w(4))
    csr_valid_q=Wire(U.w(4))
    branch_valid_q=Wire(U.w(4))

    #logic [TRANS_ID_BITS-1:0] trans_id_n, trans_id_q;
    #fu_op operator_n, operator_q; # operation to perform
    #fu_t  fu_n,       fu_q; # functional unit to use

    # forwarding signals
    forward_rs1=Wire(U.w(4)) 
    forward_rs2=Wire(U.w(4)) 
    forward_rs3=Wire(U.w(4))

    # original instruction stored in tval
    # riscv::instruction_t orig_instr;
    # orig_instr <<= riscv::instruction_t'(issue_instr_i.ex.tval[31:0]);

    # ID <-> EX registers
    fu_data_o.operand_a <<= operand_a_q
    fu_data_o.operand_b <<= operand_b_q
    fu_data_o.fu        <<= fu_q
    fu_data_o.operator  <<= operator_q
    fu_data_o.trans_id  <<= trans_id_q
    fu_data_o.imm       <<= imm_q
    alu_valid_o         <<= alu_valid_q
    branch_valid_o      <<= branch_valid_q
    lsu_valid_o         <<= lsu_valid_q
    csr_valid_o         <<= csr_valid_q
    mult_valid_o        <<= mult_valid_q
    fpu_valid_o         <<= fpu_valid_q
    fpu_fmt_o           <<= fpu_fmt_q
    fpu_rm_o            <<= fpu_rm_q

    # ---------------
    # Issue Stage
    # ---------------

    # select the right busy signal
    # this obviously depends on the functional unit we need
    # always_comb begin : unit_busy
    with when(issue_instr_i.fu == NONE):
        fu_busy = U.w(1)(0)
    with elsewhen(issue_instr_i.fu == ALU | issue_instr_i.fu == CTRL_FLOW | issue_instr_i.fu == CSR | issue_instr_i.fu == MULT):
        fu_busy = ~flu_ready_i
    with elsewhen(issue_instr_i == FPU | issue_instr_i == FPU_VEC):
        fu_busy = ~fpu_ready_i
    with elsewhen(issue_instr_i == LOAD | issue_instr_i == STORE):
        fu_busy = ~lsu_ready_i
    with otherwise():
        fu_busy = U.w(1)(0)

    # ---------------
    # Register stage
    # ---------------
    # check that all operands are available, otherwise stall
    # forward corresponding register
    #TODO: always_comb begin : operands_available
    stall = U.w(1)(0)
    # operand forwarding signals
    forward_rs1 = U.w(1)(0)
    forward_rs2 = U.w(1)(0)
    forward_rs3 = U.w(1)(0); # FPR only
    # poll the scoreboard for those values
    rs1_o = issue_instr_i.rs1
    rs2_o = issue_instr_i.rs2
    rs3_o = issue_instr_i.result[REG_ADDR_SIZE-1:0]; # rs3 is encoded in imm field

    # 0. check that we are not using the zimm type in RS1
    #    as this is an immediate we do not have to wait on anything here
    # 1. check if the source registers are clobbered --> check appropriate clobber list (gpr/fpr)
    # 2. poll the scoreboard
    with when(not issue_instr_i.use_zimm and Mux(is_rs1_fpr(issue_instr_i.op), rd_clobber_fpr_i[issue_instr_i.rs1] != NONE, rd_clobber_gpr_i[issue_instr_i.rs1] != NONE)):
        # check if the clobbering instruction is not a CSR instruction, CSR instructions can only
        # be fetched through the register file since they can't be forwarded
        # if the operand is available, forward it. CSRs don't write to/from FPR
        with when(rs1_valid_i & Mux(is_rs1_fpr(issue_instr_i.op), U.w(1)(1), rd_clobber_gpr_i[issue_instr_i.rs1] != CSR)):
            forward_rs1 = U.w(1)(1)
        with otherwise(): # the operand is not available -> stall
            stall = U.w(1)(1)

    with when(Mux(is_rs2_fpr(issue_instr_i.op), rd_clobber_fpr_i[issue_instr_i.rs2] != NONE, rd_clobber_gpr_i[issue_instr_i.rs2] != NONE)):
        # if the operand is available, forward it. CSRs don't write to/from FPR
        with when(rs2_valid_i & Mux(is_rs2_fpr(issue_instr_i.op), U.w(1)(1), rd_clobber_gpr_i[issue_instr_i.rs2] != CSR)):
            forward_rs2 = U.w(1)(1)
        with otherwise(): # the operand is not available -> stall
            stall = U.w(1)(1)

    with when(is_imm_fpr(issue_instr_i.op) & rd_clobber_fpr_i[issue_instr_i.result[REG_ADDR_SIZE-1:0]] != NONE):
        # if the operand is available, forward it. CSRs don't write to/from FPR so no need to check
        with when(rs3_valid_i):
            forward_rs3 = U.w(1)(1)
        with otherwise(): # the operand is not available -> stall
            stall = U.w(1)(1)

  return issue_read_operands()


if __name__ == '__main__':
    Emitter.dumpVerilog(Emitter.dump(Emitter.emit(issue_read_operands(2)), "issue_read_operands.fir"))