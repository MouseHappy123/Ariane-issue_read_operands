from pyhcl import *
VLEN = 64

# --------------------
# Instruction Types
# --------------------

instruction_t = Bundle(
    instr=Vec(32, U.w(4)),
    rtype=rtype_t,
    r4type=r4type_t,
    rftype=rftype_t,
    rvftype=rvftype_t,
    itype=itype_t,
    stype=stype_t,
    utype=utype_t,
    atype=atype_t
)