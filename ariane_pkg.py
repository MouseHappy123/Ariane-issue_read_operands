from math import ceil, log2
NONE      = 0
LOAD      = 1
STORE     = 2
ALU       = 3
CTRL_FLOW = 4
MULT      = 5
CSR       = 6
FPU       = 7
FPU_VEC   = 8

fu_t = {}
fu_data_t = {}
fu_op = {}
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
branchpredict_sbe_t = {}
scoreboard_entry_t = {}
def is_rs1_fpr(): ...
def is_rs2_fpr(): ...
def is_imm_fpr(): ...

rd_clobber_fpr_i = []
rd_clobber_gpr_i = []

REG_ADDR_SIZE = 6
FLEN = 64

NR_SB_ENTRIES = 8 # number of scoreboard entries
TRANS_ID_BITS = ceil(log2(NR_SB_ENTRIES))
# depending on the number of scoreboard entries we need that many bits
# to uniquely identify the entry in the scoreboard