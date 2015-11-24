--------------------------------------------------------------------------------
--
-- LAB #5 - Processor Elements
--
--------------------------------------------------------------------------------

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;
USE IEEE.STD_LOGIC_UNSIGNED.ALL;

ENTITY SmallBusMux2to1 IS
	PORT(selector	: IN STD_LOGIC;
	     In0, In1	: IN STD_LOGIC_VECTOR(4 DOWNTO 0);
	     Result	: OUT STD_LOGIC_VECTOR(4 DOWNTO 0) );
END ENTITY SmallBusMux2to1;

ARCHITECTURE switching OF SmallBusMux2to1 IS
BEGIN
    WITH selector SELECT
	Result <= In0 WHEN '0',
		  In1 WHEN OTHERS;
END ARCHITECTURE switching;

--------------------------------------------------------------------------------
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;
USE IEEE.STD_LOGIC_UNSIGNED.ALL;

ENTITY BusMux2to1 IS
	PORT(selector	: IN STD_LOGIC;
	     In0, In1	: IN STD_LOGIC_VECTOR(31 DOWNTO 0);
	     Result	: OUT STD_LOGIC_VECTOR(31 DOWNTO 0) );
END ENTITY BusMux2to1;


ARCHITECTURE selection OF BusMux2to1 IS
BEGIN
	WITH selector SELECT
		Result <= 	In0 WHEN '0',
				In1 WHEN OTHERS;
END ARCHITECTURE selection;

--------------------------------------------------------------------------------
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;
USE IEEE.STD_LOGIC_UNSIGNED.ALL;

ENTITY Control IS
    PORT ( opcode 	: IN  STD_LOGIC_VECTOR (5 DOWNTO 0);
           clk 		: IN  STD_LOGIC;
           RegDst 	: OUT  STD_LOGIC;
           Branch 	: OUT  STD_LOGIC;
           MemRead 	: OUT  STD_LOGIC;
           MemtoReg 	: OUT  STD_LOGIC;
           ALUOp 		: OUT  STD_LOGIC_VECTOR(1 DOWNTO 0);
           MemWrite 	: OUT  STD_LOGIC;
           ALUSrc 	: OUT  STD_LOGIC;
           RegWrite 	: OUT  STD_LOGIC);
END Control;

ARCHITECTURE Boss OF Control IS
BEGIN

			-- selects register to be written to
			-- instruction(15-11) when = '1'
			-- instruction(20-16) when = '0'
	RegDst <=	'1' WHEN opcode(5 DOWNTO 0) = "000000" ELSE
			'0';

			-- defines source of 1st input to MUX(M2)
			-- registers for '0' (R-type)
			-- sign extender for '1' (I-type)
	ALUSrc <=	'0' WHEN opcode(5 DOWNTO 0) = "000000" ELSE
			'1';

			-- flag bit for LW op
			-- input to 32-bit MZ mux
	MemtoReg <=	'1' WHEN opcode(5 DOWNTO 0) = "100011" ELSE
			'0';
			
			-- flag bit for register write
			-- '0' for SW op
			-- else writes on clock = '0' (between cycles)
	RegWrite <=	'0' WHEN opcode(5 DOWNTO 0) = "101011" ELSE
			not(clk);
			
			-- flag bit for lw op
			-- toggles DataMemRead bit in processor
	MemRead <=	'1' WHEN opcode(5 DOWNTO 0) = "100011" ELSE
			'0';

			-- flag bit for SW op
			-- toggles DataMemWrite bit in processor
	MemWrite <=	'1' WHEN opcode(5 DOWNTO 0) = "101011" ELSE
			'0';
			
			-- In to ALU control
			-- Defines which type of op to be performed
	ALUOp <=	"01" WHEN opcode(5 DOWNTO 0) = "000000" ELSE -- R-Type
			"10" WHEN opcode(5 DOWNTO 0) = "001101" ELSE -- or, ori
			"00"; -- LW, SW
END Boss;

--------------------------------------------------------------------------------

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;
USE IEEE.STD_LOGIC_UNSIGNED.ALL;

ENTITY ALUControl IS
	PORT(op		: IN STD_LOGIC_VECTOR(1 DOWNTO 0);
	     funct	: IN STD_LOGIC_VECTOR(5 DOWNTO 0);
	     aluctrl	: OUT STD_LOGIC_VECTOR(4 DOWNTO 0); 
	     ShiftReg	: OUT STD_LOGIC);
END ENTITY ALUControl;

ARCHITECTURE bossy OF ALUControl IS

	SIGNAL tempctrl: STD_LOGIC_VECTOR(4 DOWNTO 0);

BEGIN
	tempctrl <= 	"00000" WHEN op = "00" ELSE
			"00011" WHEN op = "10" ELSE
			"00000" WHEN op = "01" and funct = "100000" ELSE -- add op
			"00001" WHEN op = "01" and funct = "100010" ELSE -- sub op
			"00010" WHEN op = "01" and funct = "100100" ELSE --  or op
			"00011" WHEN op = "01" and funct = "100101" ELSE -- ori op
			"00100" WHEN op = "01" and funct = "000000" ELSE -- srl op
			"00101" WHEN op = "01" and funct = "000010" ELSE -- sll op
			"11111";	

	ShiftReg <= 	'1' WHEN tempctrl = "00100" or tempctrl = "00101" ELSE
			'0';

	aluctrl <= tempctrl;

END ARCHITECTURE bossy;

----------------------------------------------------------------------------------------------------------------------------------------------------------------
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;
USE IEEE.STD_LOGIC_UNSIGNED.ALL;

ENTITY Registers IS
    PORT(ReadReg1		: IN STD_LOGIC_VECTOR(4 DOWNTO 0);
         ReadReg2		: IN STD_LOGIC_VECTOR(4 DOWNTO 0); 
         WriteReg		: IN STD_LOGIC_VECTOR(4 DOWNTO 0);
			WriteData	: IN STD_LOGIC_VECTOR(31 DOWNTO 0);
			WriteCmd		: IN STD_LOGIC;
			ReadData1	: OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
			ReadData2	: OUT STD_LOGIC_VECTOR(31 DOWNTO 0));
END ENTITY Registers;



ARCHITECTURE remember OF Registers IS

	COMPONENT register32
  	    port(datain		: IN STD_LOGIC_VECTOR(31 DOWNTO 0);
				enout32, enout16, enout8
								: IN STD_LOGIC;
				writein32, writein16, writein8
								: IN STD_LOGIC;
				dataout		: OUT STD_LOGIC_VECTOR(31 DOWNTO 0));

	END COMPONENT;

	SIGNAL rzero, at, v0, v1, a0, a1, a2, a3, t0, t1, t2, t3, t4, t5, t6, t7,
				s0, s1, s2, s3, s4, s5, s6, s7, t8, t9, k0, k1,
				gp, sp, fp, ra: STD_LOGIC_VECTOR(31 DOWNTO 0);

	SIGNAL writeit		: STD_LOGIC_VECTOR(31 DOWNTO 0);
BEGIN

	zero: register32 PORT MAP(WriteData,'0','0','0',writeit(0),'0','0',rzero);
	rAT: 	register32 PORT MAP(WriteData,'0','0','0',writeit(1),'0','0',at);
	rV0: register32 PORT MAP(WriteData,'0','0','0',writeit(2),'0','0',v0);
	rV1: register32 PORT MAP(WriteData,'0','0','0',writeit(3),'0','0',v1);
	rA0: register32 PORT MAP(WriteData,'0','0','0',writeit(4),'0','0',a0);
	rA1: register32 PORT MAP(WriteData,'0','0','0',writeit(5),'0','0',a1);
	rA2: register32 PORT MAP(WriteData,'0','0','0',writeit(6),'0','0',a2);
	rA3: register32 PORT MAP(WriteData,'0','0','0',writeit(7),'0','0',a3);
	rT0: register32 PORT MAP(WriteData,'0','0','0',writeit(8),'0','0',t0);
	rT1: register32 PORT MAP(WriteData,'0','0','0',writeit(9),'0','0',t1);
	rT2: register32 PORT MAP(WriteData,'0','0','0',writeit(10),'0','0',t2);
	rT3: register32 PORT MAP(WriteData,'0','0','0',writeit(11),'0','0',t3);
	rT4: register32 PORT MAP(WriteData,'0','0','0',writeit(12),'0','0',t4);
	rT5: register32 PORT MAP(WriteData,'0','0','0',writeit(13),'0','0',t5);
	rT6: register32 PORT MAP(WriteData,'0','0','0',writeit(14),'0','0',t6);
	rT7: register32 PORT MAP(WriteData,'0','0','0',writeit(15),'0','0',t7);	
	rS0: register32 PORT MAP(WriteData,'0','0','0',writeit(16),'0','0',s0);
	rS1: register32 PORT MAP(WriteData,'0','0','0',writeit(17),'0','0',s1);
	rS2: register32 PORT MAP(WriteData,'0','0','0',writeit(18),'0','0',s2);
	rS3: register32 PORT MAP(WriteData,'0','0','0',writeit(19),'0','0',s3);
	rS4: register32 PORT MAP(WriteData,'0','0','0',writeit(20),'0','0',s4);
	rS5: register32 PORT MAP(WriteData,'0','0','0',writeit(21),'0','0',s5);
	rS6: register32 PORT MAP(WriteData,'0','0','0',writeit(22),'0','0',s6);
	rS7: register32 PORT MAP(WriteData,'0','0','0',writeit(23),'0','0',s7);
	rT8: register32 PORT MAP(WriteData,'0','0','0',writeit(24),'0','0',t8);
	rT9: register32 PORT MAP(WriteData,'0','0','0',writeit(25),'0','0',t9);
	rK0: register32 PORT MAP(WriteData,'0','0','0',writeit(26),'0','0',k0);
	rK1: register32 PORT MAP(WriteData,'0','0','0',writeit(27),'0','0',k1);
	rGp: register32 PORT MAP(WriteData,'0','0','0',writeit(28),'0','0',gp);
	rSp: register32 PORT MAP(WriteData,'0','0','0',writeit(29),'0','0',sp);
	rFp: register32 PORT MAP(WriteData,'0','0','0',writeit(30),'0','0',fp);
	rRa: register32 PORT MAP(WriteData,'0','0','0',writeit(31),'0','0',ra);
	

	writeit <= 	x"00000001" WHEN WriteReg = "00001" and WriteCmd = '1' ELSE --01
			x"00000002" WHEN WriteReg = "00010" and WriteCmd = '1' ELSE --02
			x"00000003" WHEN WriteReg = "00011" and WriteCmd = '1' ELSE --03
			x"00000004" WHEN WriteReg = "00100" and WriteCmd = '1' ELSE --04
			x"00000005" WHEN WriteReg = "00101" and WriteCmd = '1' ELSE --05
			x"00000006" WHEN WriteReg = "00110" and WriteCmd = '1' ELSE --06
			x"00000007" WHEN WriteReg = "00111" and WriteCmd = '1' ELSE --07
			x"00000008" WHEN WriteReg = "01000" and WriteCmd = '1' ELSE --08
			x"00000009" WHEN WriteReg = "01001" and WriteCmd = '1' ELSE --09
			x"0000000A" WHEN WriteReg = "01010" and WriteCmd = '1' ELSE --10
			x"0000000B" WHEN WriteReg = "01011" and WriteCmd = '1' ELSE --11
			x"0000000C" WHEN WriteReg = "01100" and WriteCmd = '1' ELSE --12
			x"0000000D" WHEN WriteReg = "01101" and WriteCmd = '1' ELSE --13
			x"0000000E" WHEN WriteReg = "01110" and WriteCmd = '1' ELSE --14
			x"0000000F" WHEN WriteReg = "01111" and WriteCmd = '1' ELSE --15	
			x"00000010" WHEN WriteReg = "10000" and WriteCmd = '1' ELSE --16
			x"00000011" WHEN WriteReg = "10001" and WriteCmd = '1' ELSE --17
			x"00000012" WHEN WriteReg = "10010" and WriteCmd = '1' ELSE --18
			x"00000013" WHEN WriteReg = "10011" and WriteCmd = '1' ELSE --19
			x"00000014" WHEN WriteReg = "10100" and WriteCmd = '1' ELSE --20
			x"00000015" WHEN WriteReg = "10101" and WriteCmd = '1' ELSE --21
			x"00000016" WHEN WriteReg = "10110" and WriteCmd = '1' ELSE --22
			x"00000017" WHEN WriteReg = "10111" and WriteCmd = '1' ELSE --23
			x"00000018" WHEN WriteReg = "11000" and WriteCmd = '1' ELSE --24
			x"00000019" WHEN WriteReg = "11001" and WriteCmd = '1' ELSE --25
			x"0000001A" WHEN WriteReg = "11010" and WriteCmd = '1' ELSE --26
			x"0000001B" WHEN WriteReg = "11011" and WriteCmd = '1' ELSE --27
			x"0000001C" WHEN WriteReg = "11100" and WriteCmd = '1' ELSE --28
			x"0000001D" WHEN WriteReg = "11101" and WriteCmd = '1' ELSE --29
			x"0000001E" WHEN WriteReg = "11110" and WriteCmd = '1' ELSE --30
			x"0000001F" WHEN WriteReg = "11111" and WriteCmd = '1' ELSE --31
			x"00000000"; --0
			
	ReadData1 <= 	
			at WHEN ReadReg1 = "00001" ELSE --1
			v0 WHEN ReadReg1 = "00010" ELSE --2
			v1 WHEN ReadReg1 = "00011" ELSE --3
			a0 WHEN ReadReg1 = "00100" ELSE --4
			a1 WHEN ReadReg1 = "00101" ELSE --5
			a2 WHEN ReadReg1 = "00110" ELSE --6
			a3 WHEN ReadReg1 = "00111" ELSE --7
			t0 WHEN ReadReg1 = "01000" ELSE --8
			t1 WHEN ReadReg1 = "01001" ELSE --9
			t2 WHEN ReadReg1 = "01010" ELSE --10
			t3 WHEN ReadReg1 = "01011" ELSE --11
			t4 WHEN ReadReg1 = "01100" ELSE --12
			t5 WHEN ReadReg1 = "01101" ELSE --13
			t6 WHEN ReadReg1 = "01110" ELSE --14
			t7 WHEN ReadReg1 = "01111" ELSE --15
			s0 WHEN ReadReg1 = "10000" ELSE --16
			s1 WHEN ReadReg1 = "10001" ELSE --17
			s2 WHEN ReadReg1 = "10010" ELSE --18
			s3 WHEN ReadReg1 = "10011" ELSE --19
			s4 WHEN ReadReg1 = "10100" ELSE --20
			s5 WHEN ReadReg1 = "10101" ELSE --21
			s6 WHEN ReadReg1 = "10110" ELSE --22
			s7 WHEN ReadReg1 = "10111" ELSE --23
			t8 WHEN ReadReg1 = "11000" ELSE --24
			t9 WHEN ReadReg1 = "11001" ELSE --25
			k0 WHEN ReadReg1 = "11010" ELSE --26
			k1 WHEN ReadReg1 = "11011" ELSE --27
			gp WHEN ReadReg1 = "11100" ELSE --28
			sp WHEN ReadReg1 = "11101" ELSE --29
			fp WHEN ReadReg1 = "11110" ELSE --30
			ra WHEN ReadReg1 = "11111" ELSE --31
			rzero; --0

	ReadData2 <= 	
			at WHEN ReadReg2 = "00001" ELSE --1
			v0 WHEN ReadReg2 = "00010" ELSE --2
			v1 WHEN ReadReg2 = "00011" ELSE --3
			a0 WHEN ReadReg2 = "00100" ELSE --4
			a1 WHEN ReadReg2 = "00101" ELSE --5
			a2 WHEN ReadReg2 = "00110" ELSE --6
			a3 WHEN ReadReg2 = "00111" ELSE --7
			t0 WHEN ReadReg2 = "01000" ELSE --8
			t1 WHEN ReadReg2 = "01001" ELSE --9
			t2 WHEN ReadReg2 = "01010" ELSE --10
			t3 WHEN ReadReg2 = "01011" ELSE --11
			t4 WHEN ReadReg2 = "01100" ELSE --12
			t5 WHEN ReadReg2 = "01101" ELSE --13
			t6 WHEN ReadReg2 = "01110" ELSE --14
			t7 WHEN ReadReg2 = "01111" ELSE --15
			s0 WHEN ReadReg2 = "10000" ELSE --16
			s1 WHEN ReadReg2 = "10001" ELSE --17
			s2 WHEN ReadReg2 = "10010" ELSE --18
			s3 WHEN ReadReg2 = "10011" ELSE --19
			s4 WHEN ReadReg2 = "10100" ELSE --20
			s5 WHEN ReadReg2 = "10101" ELSE --21
			s6 WHEN ReadReg2 = "10110" ELSE --22
			s7 WHEN ReadReg2 = "10111" ELSE --23
			t8 WHEN ReadReg2 = "11000" ELSE --24
			t9 WHEN ReadReg2 = "11001" ELSE --25
			k0 WHEN ReadReg2 = "11010" ELSE --26
			k1 WHEN ReadReg2 = "11011" ELSE --27
			gp WHEN ReadReg2 = "11100" ELSE --28
			sp WHEN ReadReg2 = "11101" ELSE --29
			fp WHEN ReadReg2 = "11110" ELSE --30
			ra WHEN ReadReg2 = "11111" ELSE --31
			rzero; --0

END remember;

----------------------------------------------------------------------------------------------------------------------------------------------------------------
