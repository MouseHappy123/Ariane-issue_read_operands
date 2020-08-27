**发射阶段（Issue Stage）**模块的重构，顶层模块为**issue_stage.sv**，包含的子模块有：

- 寄存器重命名模块：**re_name.sv**

- 计分板模块：**scoreboard.sv**

- 发射模块：**issue_read_operands.sv**

  - 两个寄存器组的实现：整数寄存器以及浮点数寄存器，都是使用**ariane_regfile.sv**模块（基于锁存器的寄存器组）

    

Ariane处理器的主要参数：

- 64位单核，顺序发射，乱序执行，顺序提交；

- 支持硬件乘法、除法器（M），浮点运算器（FPU，F、D），也就是说Ariane支持的指令集为RV64G；

- 支持压缩指令子集（C）以及完整的处理器特权模式级别；

- 6级流水线：

  - 前端部分（Frontend）
    - (1) PC生成（PC Generation）
    - (2) 指令获取（Instruction Fetch）
  - (3) 指令译码（Instruction Decode）
  - (4) 发射（Issue）
  - (5) 执行（Execute）
  - (6) 提交（Commit）

- 支持分支预测策略，硬件实现有：
 
  - BHT：分支历史表，用于预测当前分支指令是否taken（即是否执行分支）
  - BTB：分支目标缓冲，用于判断当前指令是否是分支指令
  - RAS：返回地址堆栈，用于Return类指令获取返回地址

- 支持虚拟内存：硬件实现有：

  - MMU：内存管理单元，内实现有：
    - TLB：转换后备缓冲器
    - PTW：硬件实现的Page Table Walker，当Cache Miss的时候会使用到

- 支持乱序执行，硬件实现有：

  - Scoreboard：计分板，用于追踪已经发射的指令，以及指令的数据依赖情况。当指令被发射后，就会在计分板内被注册（开始追踪），计分板中还实现有ROB：
    - ROB：Reorder Buffer，用于重排列乱序Retire后的指令，使其能够被按照发射的顺序提交，同时也为了能够定位异常等。

- 功能部件有：

  - ALU：算术逻辑单元，覆盖几乎所有的基本RISC-V算术逻辑指令

  - LSU：存储加载单元，用于管理整数、浮点数在cache的存储与加载以及存储器原子操作

  - FPU：浮点运算单元

  - Branch Unit：ALU的扩展模块，专用于计算跳转地址以及判断跳转是否执行

  - CSR：控制和状态寄存器，专用于特权模式，用于记录处理器的运行状态以及配置信息

  - Multiplier/Divider：硬件乘法器和除法器

    
