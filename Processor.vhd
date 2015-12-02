--------------------------------------------------------------------------------
--
-- LAB #5 - Processor 
--
--------------------------------------------------------------------------------

 
--------------------------------------------------------------------------------

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;
USE IEEE.STD_LOGIC_UNSIGNED.ALL;

ENTITY Processor IS

    PORT (	instruction 	: IN  STD_LOGIC_VECTOR (31 DOWNTO 0);
				DataMemdatain 	: IN  STD_LOGIC_VECTOR (31 DOWNTO 0);
				DataMemdataout	: OUT STD_LOGIC_VECTOR (31 DOWNTO 0);
				InstMemAddr 	: OUT STD_LOGIC_VECTOR (31 DOWNTO 0);
				DataMemAddr 	: OUT STD_LOGIC_VECTOR (31 DOWNTO 0);
				DataMemRead 	: OUT STD_LOGIC;
				DataMemWrite 	: OUT STD_LOGIC;
				clock 			: IN  STD_LOGIC);
				
END Processor;

architecture holistic of Processor IS
	
	--signals
	
	SIGNAL 	RegDst1, Branch1, MemRead1, MemtoReg1, 
				MemWrite1, ALUSrc,Regwrite, zero1, shiftSel
							: STD_LOGIC;
	
	SIGNAL 	ALUOp1	: STD_LOGIC_VECTOR(1 DOWNTO 0);
	
	SIGNAL 	aluctrl1, intoReg1, intowrite
							: STD_LOGIC_VECTOR(4 DOWNTO 0);
	
	SIGNAL 	rdata1, rdata2, intowriteD, extendsig,
				extendsig2, tempaluin2,aluin2, alures,
				tempMemAddr, add_in, add_out
							: STD_LOGIC_VECTOR(31 DOWNTO 0);
	
	--components
	COMPONENT Control
		PORT(opcode		: IN  STD_LOGIC_VECTOR (5 DOWNTO 0);
	        clk			: IN  STD_LOGIC;
		     RegDst		: OUT STD_LOGIC;
		     Branch		: OUT STD_LOGIC;
		     MemRead	: OUT STD_LOGIC;
		     MemtoReg 	: OUT STD_LOGIC;
		     ALUOp 		: OUT STD_LOGIC_VECTOR(1 DOWNTO 0);
		     MemWrite 	: OUT STD_LOGIC;
		     ALUSrc 	: OUT STD_LOGIC;
		     RegWrite 	: OUT STD_LOGIC);
	END COMPONENT;

	COMPONENT ALUControl
	   PORT(	op			: IN	STD_LOGIC_VECTOR(1 DOWNTO 0);
				funct		: IN	STD_LOGIC_VECTOR(5 DOWNTO 0);
				aluctrl	: OUT STD_LOGIC_VECTOR(4 DOWNTO 0);
				ShiftReg	: OUT STD_LOGIC);
	END COMPONENT;

	COMPONENT ALU
		PORT(	DataIn1	: IN	STD_LOGIC_VECTOR(31 DOWNTO 0);
				DataIn2	: IN	STD_LOGIC_VECTOR(31 DOWNTO 0);
				Control	: IN	STD_LOGIC_VECTOR(4 DOWNTO 0);
				Zero		: OUT STD_LOGIC;
				ALUResult: OUT STD_LOGIC_VECTOR(31 DOWNTO 0) );
	END COMPONENT;
	
	COMPONENT Registers
	   PORT(	ReadReg1	: IN	STD_LOGIC_VECTOR(4 DOWNTO 0); 
				ReadReg2	: IN	STD_LOGIC_VECTOR(4 DOWNTO 0); 
				WriteReg	: IN	STD_LOGIC_VECTOR(4 DOWNTO 0);
				WriteData: IN 	STD_LOGIC_VECTOR(31 DOWNTO 0);
				WriteCmd	: IN 	STD_LOGIC;
				ReadData1: OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
				ReadData2: OUT STD_LOGIC_VECTOR(31 DOWNTO 0));
	END COMPONENT;
	
	COMPONENT BusMux2to1
		PORT(	selector	: IN 	STD_LOGIC;
				In0, In1	: IN 	STD_LOGIC_VECTOR(31 DOWNTO 0);
				Result	: OUT STD_LOGIC_VECTOR(31 DOWNTO 0) );
	END COMPONENT;
	
	COMPONENT SmallbusMux2to1
		PORT(	selector	: IN 	STD_LOGIC;
				In0, In1	: IN 	STD_LOGIC_VECTOR(4 DOWNTO 0);
				Result	: OUT STD_LOGIC_VECTOR(4 DOWNTO 0));
	END COMPONENT;

	COMPONENT adder_subtracter IS
		PORT(	datain_a	: IN 	STD_LOGIC_vector(31 DOWNTO 0);
				datain_b	: IN 	STD_LOGIC_vector(31 DOWNTO 0);
				add_sub	: IN 	STD_LOGIC;
				dataout	: OUT STD_LOGIC_vector(31 DOWNTO 0);
				co			: OUT STD_LOGIC);		
	END COMPONENT;

	
BEGIN

	--
	--
	extendsig <= 	"1111111111111111" & instruction(15 DOWNTO 0) WHEN instruction(15) = '1' ELSE
			"0000000000000000" & instruction(15 DOWNTO 0);	

	-- shift amount. not sure why we need to signify a negative number if inst(10) = '1'
	extendsig2 <= 	"111111111111111111111111111" & instruction(10 DOWNTO 6) WHEN instruction(10) = '1' ELSE
			"000000000000000000000000000" & instruction(10 DOWNTO 6);	

	CO: Control PORT MAP(instruction(31 DOWNTO 26),clock,RegDst1,Branch1,MemRead1,MemtoReg1,ALUOp1,MemWrite1,ALUSrc,Regwrite);
	
	-- Selects register to read from based on whether we are doing a shift operation.
	-- reads from register at inst(25-21) for shiftSel = '0', inst(20-16) for shiftSel = '1'
	-- shiftSel is set by the ALUControl unit 
	RM: SmallbusMux2to1 PORT MAP(shiftSel,instruction(25 DOWNTO 21),instruction(20 DOWNTO 16), intoReg1);
	
	-- Selects 2nd input to ALU, based on whether we are doing a shift operation
	-- 1st input is from MX mux, 2nd input is sign-extended shift amount 
	-- shiftSel is set by the ALUControl unit 
	M2: BusMux2to1 PORT MAP(shiftSel,tempaluin2,extendsig2,aluin2);

	AL: ALUControl PORT MAP(ALUOp1,instruction(5 DOWNTO 0),aluctrl1,shiftSel);
	
	-- Selects write register. instruction (15 downto 11) for RegDst1 = '1', 
	--	instruction (20 downto 16) for RegDst1 = '0'
	-- RegDst1 is set by control unit = '1' for R-type ops
	-- '0' for others.
	MU: SmallbusMux2to1 PORT MAP(RegDst1,instruction(20 DOWNTO 16),instruction(15 DOWNTO 11),intowrite);
	
	-- 32 x 32-bit register bank
	RE: Registers PORT MAP(intoReg1,instruction(20 DOWNTO 16),intowrite,intowriteD,Regwrite,rdata1,rdata2);

	-- 32-bit mux
	-- Selects 1st input to M2 mux, based on ALUSrc.
	-- ALUSrc is '0' for R-type instructions, '1' for others
	-- output is register data for R-type, 
	-- sign extended lower 16 bits of instruction for I-type
	MX: BusMux2to1 PORT MAP(ALUSrc, rdata2, extendsig,tempaluin2);
	
	
	AU: ALU PORT MAP(rdata1,aluin2,aluctrl1, zero1, tempMemAddr);
	
	-- 32-bit mux
	-- Selects Data to be written to registers in LW op
	-- Input : tempMemAddr [ALU Output], DataMemDatain [Register Contents]
	-- Output: intowriteD [Write Data -- is written to location specified by 5-bit MU mux]
	-- Control: MemtoReg1 ['1' for LW op, '0' for others]
	MZ: BusMux2to1 PORT MAP(MemtoReg1,tempMemAddr,DataMemdatain,intowriteD);

	DataMemAddr <= tempMemAddr;
	DataMemWrite <= MemWrite1;
	DataMemdataout <= rdata2;
	DataMemRead <= MemRead1;
	
END holistic;
