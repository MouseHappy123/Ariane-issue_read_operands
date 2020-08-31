from pyhcl import *
from functools import reduce
from ariane_pkg import *
import riscv_pkg as riscv

def issue_read_operands(NR_COMMIT_PORTS: int):
  class issue_read_operands(Module):
    io = IO(
      clk_i=Input(U.w(4)), # Clock
      rst_ni=Input(U.w(4)), # Asynchronous reset active low
      # flush
      flush_i=Input(U.w(4)),
      # coming from rename
      issue_instr_i=Input(scoreboard_entry_t),
      issue_instr_valid_i=Input(U.w(4)),
      issue_ack_o=Output(U.w(4)),
      # lookup rd in scoreboard
      rs1_o=Output(Vec(REG_ADDR_SIZE, U.w(4))),
      rs1_i=Input(Vec(64,U.w(4)), U.w(4)),
      rs1_valid_i=Input(U.w(4)),
      rs2_o=Output(Vec(REG_ADDR_SIZE, U.w(4))),
      rs2_i=Input(Vec(64,U.w(4))),
      rs2_valid_i=Input(U.w(4)),
      rs3_o=Output(Vec(REG_ADDR_SIZE, U.w(4))),
      rs3_i=Input(Vec(FLEN, U.w(4))),
      rs3_valid_i=Input(U.w(4)),
      # get clobber input
      rd_clobber_gpr_i=Input(Vec(2**REG_ADDR_SIZE-1, fu_t)),
      rd_clobber_fpr_i=Input(Vec(2**REG_ADDR_SIZE-1, fu_t)),
      # To FU, just single issue for now
      fu_data_o=Output(fu_data_t),
      pc_o=Output(Vec(riscv.VLEN), U.w(4)),
      is_compressed_instr_o=Output(U.w(4)),
      # ALU 1
      flu_ready_i=Input(U.w(4)),      # Fixed latency unit ready to accept a new request
      alu_valid_o=Output(U.w(4)),      # Output is valid
      # Branches and Jumps
      branch_valid_o=Output(U.w(4)),   #  this is a valid branch instruction
      branch_predict_o = Output(branchpredict_sbe_t),
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
    operand_c_regfile=Wire(Vec(FLEN, U.w(4))) # third operand only from fp regfile
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

    trans_id_n = Wire(Vec(TRANS_ID_BITS), U.w(4))
    trans_id_q = Wire(Vec(TRANS_ID_BITS), U.w(4))
    # operation to perform
    operator_n = fu_op
    operator_q = fu_op
    # functional unit to use
    fu_n = fu_t
    fu_q = fu_t

    # forwarding signals
    forward_rs1=Wire(U.w(4)) 
    forward_rs2=Wire(U.w(4)) 
    forward_rs3=Wire(U.w(4))

    # original instruction stored in tval
    orig_instr = riscv.instruction_t
    #TODO
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
    # Forwarding/Output MUX
    # always_comb begin : forwarding_operand_select
    # default is regfiles (gpr or fpr)
    operand_a_n = operand_a_regfile
    operand_b_n = operand_b_regfile
    # immediates are the third operands in the store case
    # for FP operations, the imm field can also be the third operand from the regfile
    imm_n      = Mux(is_imm_fpr(issue_instr_i.op), operand_c_regfile, issue_instr_i.result)
    trans_id_n = issue_instr_i.trans_id
    fu_n       = issue_instr_i.fu
    operator_n = issue_instr_i.op
    # or should we forward
    with when(forward_rs1):
        operand_a_n  = io.rs1_i

    with when(forward_rs2):
        operand_b_n  = io.rs2_i

    with when(forward_rs3):
        imm_n  = io.rs3_i

    # use the PC as operand a
    with when(issue_instr_i.use_pc):
        ...
        #TODO
        # operand_a_n = {{64-riscv.VLEN{issue_instr_i.pc[riscv.VLEN-1]}}, issue_instr_i.pc}

    # use the zimm as operand a
    with when(issue_instr_i.use_zimm):
        # zero extend operand a
        #TODO
        operand_a_n = {59'b0, issue_instr_i.rs1[4:0]};

    # or is it an immediate (including PC), this is not the case for a store and control flow instructions
    # also make sure operand B is not already used as an FP operand
    if (issue_instr_i.use_imm && (issue_instr_i.fu != STORE) && (issue_instr_i.fu != CTRL_FLOW) && !is_rs2_fpr(issue_instr_i.op)) begin
        operand_b_n = issue_instr_i.result;
    end

    # FU select, assert the correct valid out signal (in the next cycle)
    # This needs to be like this to make verilator happy. I know its ugly.
    always_ff @(posedge clk_i or negedge rst_ni) begin
      if (!rst_ni) begin
        alu_valid_q    <= 1'b0;
        lsu_valid_q    <= 1'b0;
        mult_valid_q   <= 1'b0;
        fpu_valid_q    <= 1'b0;
        fpu_fmt_q      <= 2'b0;
        fpu_rm_q       <= 3'b0;
        csr_valid_q    <= 1'b0;
        branch_valid_q <= 1'b0;
      end else begin
        alu_valid_q    <= 1'b0;
        lsu_valid_q    <= 1'b0;
        mult_valid_q   <= 1'b0;
        fpu_valid_q    <= 1'b0;
        fpu_fmt_q      <= 2'b0;
        fpu_rm_q       <= 3'b0;
        csr_valid_q    <= 1'b0;
        branch_valid_q <= 1'b0;
        # Exception pass through:
        # If an exception has occurred simply pass it through
        # we do not want to issue this instruction
        if (!issue_instr_i.ex.valid && issue_instr_valid_i && issue_ack_o) begin
            case (issue_instr_i.fu)
                ALU:
                    alu_valid_q    <= 1'b1;
                CTRL_FLOW:
                    branch_valid_q <= 1'b1;
                MULT:
                    mult_valid_q   <= 1'b1;
                FPU : begin
                    fpu_valid_q    <= 1'b1;
                    fpu_fmt_q      <= orig_instr.rftype.fmt; # fmt bits from instruction
                    fpu_rm_q       <= orig_instr.rftype.rm;  # rm bits from instruction
                end
                FPU_VEC : begin
                    fpu_valid_q    <= 1'b1;
                    fpu_fmt_q      <= orig_instr.rvftype.vfmt;         # vfmt bits from instruction
                    fpu_rm_q       <= {2'b0, orig_instr.rvftype.repl}; # repl bit from instruction
                end
                LOAD, STORE:
                    lsu_valid_q    <= 1'b1;
                CSR:
                    csr_valid_q    <= 1'b1;
                default:;
            endcase
        end
        # if we got a flush request, de-assert the valid flag, otherwise we will start this
        # functional unit with the wrong inputs
        if (flush_i) begin
            alu_valid_q    <= 1'b0;
            lsu_valid_q    <= 1'b0;
            mult_valid_q   <= 1'b0;
            fpu_valid_q    <= 1'b0;
            csr_valid_q    <= 1'b0;
            branch_valid_q <= 1'b0;
        end
      end
    end

  return issue_read_operands()


if __name__ == '__main__':
    Emitter.dumpVerilog(Emitter.dump(Emitter.emit(issue_read_operands(2)), "issue_read_operands.fir"))