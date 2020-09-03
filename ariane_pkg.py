from pyhcl import *
from math import ceil, log2

def Range(start: U, stop: U):
    for i in range(start.to_uint(), stop.to_uint()):
        yield U(i)

def define(v):
    try:
        type(eval(v))
    except NameError:
        return 0
    else:
        return 1

# https://github.com/openhwgroup/cva6/blob/v4.2.0/include/ariane_pkg.sv#L157
if define('PITON_ARIANE'):
    # Floating-point extensions configuration
    RVF = U.w(1)(1) # Is F extension enabled
    RVD = U.w(1)(1) # Is D extension enabled
else:
    # Floating-point extensions configuration
    RVF = U.w(1)(1) # Is F extension enabled
    RVD = U.w(1)(1) # Is D extension enabled
RVA = U.w(1)(1) # Is A extension enabled

# Transprecision floating-point extensions configuration
XF16    = U.w(1)(0) # Is half-precision float extension (Xf16) enabled
XF16ALT = U.w(1)(0) # Is alternative half-precision float extension (Xf16alt) enabled
XF8     = U.w(1)(0) # Is quarter-precision float extension (Xf8) enabled
XFVEC   = U.w(1)(0) # Is vectorial float extension (Xfvec) enabled

# Transprecision float unit
LAT_COMP_FP32    = U(2)
LAT_COMP_FP64    = U(3)
LAT_COMP_FP16    = U(1)
LAT_COMP_FP16ALT = U(1)
LAT_COMP_FP8     = U(1)
LAT_DIVSQRT      = U(2)
LAT_NONCOMP      = U(1)
LAT_CONV         = U(2)

# --------------------------------------
# vvvv Don't change these by hand! vvvv
FP_PRESENT = RVF | RVD | XF16 | XF16ALT | XF8

# Length of widest floating-point format
FLEN = 0
if RVD: FLEN = 64       # D ext.
elif RVF: FLEN = 32     # F ext.
elif XF16: FLEN = 16    # Xf16 ext.
elif XF16ALT: FLEN = 16 # Xf16alt ext.
elif XF8: FLEN = 8      # Xf8 ext.
else: FLEN = 1          # Unused in case of no FP

# https://github.com/openhwgroup/cva6/blob/v4.2.0/include/ariane_pkg.sv#L219
# 32 registers + 1 bit for re-naming = 6
REG_ADDR_SIZE = 6
NR_WB_PORTS = 4

class cf_t:
    NoCF = U(0)   # No control flow prediction
    Branch = U(1) # Branch
    Jump = U(2)   # Jump to address from immediate
    JumpR = U(3)  # Jump to address from registers
    Return = U(4)  # Return Address Prediction

# https://github.com/openhwgroup/cva6/blob/v4.2.0/include/ariane_pkg.sv#L349
class fu_t:
    NONE      = U(0)
    LOAD      = U(1)
    STORE     = U(2)
    ALU       = U(3)
    CTRL_FLOW = U(4)
    MULT      = U(5)
    CSR       = U(6)
    FPU       = U(7)
    FPU_VEC   = U(8)

# https://github.com/openhwgroup/cva6/blob/v4.2.0/include/ariane_pkg.sv#L431
# ---------------
# EX Stage
# ---------------
class fu_op:
    # basic ALU op
    ADD, SUB, ADDW, SUBW = U(0), U(1), U(2), U(3)
    # logic operations
    XORL, ORL, ANDL = U(4), U(5), U(6)
    # shifts
    SRA, SRL, SLL, SRLW, SLLW, SRAW = U(7), U(8), U(9), U(10), U(11), U(12)
    # comparisons
    LTS, LTU, GES, GEU, EQ, NE = U(13), U(14), U(15), U(16), U(17), U(18)
    # jumps
    JALR, BRANCH = U(19), U(20)
    # set lower than operations
    SLTS, SLTU = U(21), U(22)
    # CSR functions
    MRET, SRET, DRET, ECALL, WFI, FENCE, FENCE_I, SFENCE_VMA, CSR_WRITE, CSR_READ, CSR_SET, CSR_CLEAR = U(23), U(24), U(25), U(26), U(27), U(28), U(29), U(30), U(31), U(32), U(33), U(34)
    # LSU functions
    LD, SD, LW, LWU, SW, LH, LHU, SH, LB, SB, LBU = U(35), U(36), U(37), U(38), U(39), U(40), U(41), U(42), U(43), U(44), U(45)
    # Atomic Memory Operations
    AMO_LRW, AMO_LRD, AMO_SCW, AMO_SCD = U(46), U(47), U(48), U(49)
    AMO_SWAPW, AMO_ADDW, AMO_ANDW, AMO_ORW, AMO_XORW, AMO_MAXW, AMO_MAXWU, AMO_MINW, AMO_MINWU = U(50), U(51), U(52), U(53), U(54), U(55), U(56), U(57), U(58)
    AMO_SWAPD, AMO_ADDD, AMO_ANDD, AMO_ORD, AMO_XORD, AMO_MAXD, AMO_MAXDU, AMO_MIND, AMO_MINDU = U(59), U(60), U(61), U(62), U(63), U(64), U(65), U(66), U(67)
    # Multiplications
    MUL, MULH, MULHU, MULHSU, MULW = U(68), U(69), U(70), U(71), U(72)
    # Divisions
    DIV, DIVU, DIVW, DIVUW, REM, REMU, REMW, REMUW = U(73), U(74), U(75), U(76), U(77), U(78), U(79), U(80)
    # Floating-Point Load and Store Instructions
    FLD, FLW, FLH, FLB, FSD, FSW, FSH, FSB = U(81), U(82), U(83), U(84), U(85), U(86), U(87), U(88)
    # Floating-Point Computational Instructions
    FADD, FSUB, FMUL, FDIV, FMIN_MAX, FSQRT, FMADD, FMSUB, FNMSUB, FNMADD = U(89), U(90), U(91), U(92), U(93), U(94), U(95), U(96), U(97), U(98)
    # Floating-Point Conversion and Move Instructions
    FCVT_F2I, FCVT_I2F, FCVT_F2F, FSGNJ, FMV_F2X, FMV_X2F = U(99), U(100), U(101), U(102), U(103), U(104)
    # Floating-Point Compare Instructions
    FCMP = U(105)
    # Floating-Point Classify Instruction
    FCLASS = U(106)
    # Vectorial Floating-Point Instructions that don't directly map onto the scalar ones
    VFMIN, VFMAX, VFSGNJ, VFSGNJN, VFSGNJX, VFEQ, VFNE, VFLT, VFGE, VFLE, VFGT, VFCPKAB_S, VFCPKCD_S, VFCPKAB_D, VFCPKCD_D = U(107), U(108), U(109), U(110), U(111), U(112), U(113), U(114), U(115), U(116), U(117), U(118), U(119), U(120), U(121)


# https://github.com/openhwgroup/cva6/blob/v4.2.0/include/ariane_pkg.sv#L488
def is_rs1_fpr(op: fu_op):
    if FP_PRESENT: # makes function static for non-fp case
        if op in [
            *list(Range(fu_op.FMUL, fu_op.FNMADD)),      # Computational Operations (except ADD/SUB)
            fu_op.FCVT_F2I,                              # Float-Int Casts
            fu_op.FCVT_F2F,                              # Float-Float Casts
            fu_op.FSGNJ,                                 # Sign Injections
            fu_op.FMV_F2X,                               # FPR-GPR Moves
            fu_op.FCMP,                                  # Comparisons
            fu_op.FCLASS,                                # Classifications
            *list(Range(fu_op.VFMIN, fu_op.VFCPKCD_D)),  # Additional Vectorial FP ops
        ]:
            return U.w(1)(1)
        else:  # all other ops
            return U.w(1)(0)
    else:
        return U.w(1)(0)

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

