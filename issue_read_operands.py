from pyhcl import *
from functools import reduce

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

  return issue_read_operands()


if __name__ == '__main__':
    Emitter.dumpVerilog(Emitter.dump(Emitter.emit(issue_read_operands(2)), "issue_read_operands.fir"))