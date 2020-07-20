from enum import Enum

class fu_t(Enum):
    NONE      = 0
    LOAD      = 1
    STORE     = 2
    ALU       = 3
    CTRL_FLOW = 4
    MULT      = 5
    CSR       = 6
    FPU       = 7
    FPU_VEC   = 8
