from math import ceil, log2
from enum import Enum

fu_t = Enum()
NONE      = 0
LOAD      = 1
STORE     = 2
ALU       = 3
CTRL_FLOW = 4
MULT      = 5
CSR       = 6
FPU       = 7
FPU_VEC   = 8

fu_op = Enum()
ADD = 0

REG_ADDR_SIZE = 6
FLEN = 64

def is_rs1_fpr(): ...
def is_rs2_fpr(): ...
def is_imm_fpr(): ...
def is_rd_fpr(): ...
NR_SB_ENTRIES = 8 # number of scoreboard entries
TRANS_ID_BITS = ceil(log2(NR_SB_ENTRIES))
# depending on the number of scoreboard entries we need that many bits
# to uniquely identify the entry in the scoreboard

scoreboard_entry_t = {}
fu_data_t = {}
branchpredict_sbe_t = {}

