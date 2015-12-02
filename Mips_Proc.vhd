-----------------------------------------------------------------------------------
--Begin Mips_Proc
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;
USE IEEE.STD_LOGIC_UNSIGNED.ALL;
USE IEEE.NUMERIC_STD.ALL;

ENTITY Mips_Proc IS
		PORT(	clk_50		: IN STD_LOGIC; -- 50 MHz Clock in
				clk			: BUFFER STD_LOGIC; -- 1 Hz Clock out 
				clk_tmp		: IN STD_LOGIC;
				ledG8		: OUT STD_LOGIC; -- Clock display	
				ledR		: OUT STD_LOGIC_VECTOR(15 DOWNTO 0); -- Red LEDs, displays instruction at current address									
				sw			: IN STD_LOGIC_VECTOR(17 DOWNTO 0); -- switches 0-17
				sevD0, sevD1, sevD2, sevD3, 	
				sevD4, sevD5, sevD6, sevD7 
								: OUT STD_LOGIC_VECTOR(6 DOWNTO 0); -- 7-segment digits 0-7
				-- sram access
				sram_addr	: BUFFER STD_LOGIC_VECTOR(19 DOWNTO 0);
				sram_dq		: INOUT STD_LOGIC_VECTOR(15 DOWNTO 0);
				sram_we, sram_ce, sram_oe, sram_lb, sram_ub
							: OUT STD_LOGIC);
END Mips_Proc;


ARCHITECTURE behavior OF Mips_Proc IS

-- signals & variables
	-- these are for dealing with instruction memory 
	SIGNAL	PC			: STD_LOGIC_VECTOR(19 DOWNTO 0) := X"00000"; -- Program counter
	SIGNAL 	address 	: STD_LOGIC_VECTOR(31 DOWNTO 0); -- for output to 7seg display
	SIGNAL	instruction	: STD_LOGIC_VECTOR(31 DOWNTO 0); -- complete instruction input to 32-bit register
	SIGNAL 	inst32	: STD_LOGIC_VECTOR(31 DOWNTO 0); -- complete instruction output from 32-bit register
	SIGNAL 	output_1	: STD_LOGIC_VECTOR(15 DOWNTO 0) := X"0000"; -- display sram data on red LEDs
	SIGNAL	dMDi		: STD_LOGIC_VECTOR(31 DOWNTO 0); -- DataMemdatain
	SIGNAL	dMDo		: STD_LOGIC_VECTOR(31 DOWNTO 0); -- DataMemdataout
	SIGNAL	dMAd		: STD_LOGIC_VECTOR(31 DOWNTO 0); -- DataMemAddr
	SIGNAL	dMRd		: STD_LOGIC; -- DataMemRead
	SIGNAL	dMWr		: STD_LOGIC; -- DataMemWrite	.
	SIGNAL 	procClk	: STD_LOGIC; -- clock for processor
	SHARED VARIABLE	regEnable	: STD_LOGIC := '0'; -- toggle write enable for processor registers
	SIGNAL 	sram_wein	: STD_LOGIC := '1'; -- toggle write enable for SRAM
	-- variables for LW and SW ops
	SHARED VARIABLE dMRdTmp : STD_LOGIC;
	SHARED VARIABLE dMWrTmp : STD_LOGIC;
	SIGNAL 	displayVector	: STD_LOGIC_VECTOR(31 DOWNTO 0); -- vector for 7-segment display 
	TYPE STATE_TYPE IS (s0, s1, s2, s3, s4, s5, s6); -- typedef and signals for state machine
	SIGNAL 	current_state, next_state	: STATE_TYPE;
	SIGNAL 	inst_we: STD_LOGIC;
	SIGNAL	data_SRAM		:STD_LOGIC_VECTOR(31 DOWNTO 0);
	
-- components
	COMPONENT clock
		GENERIC(n:	INTEGER := 50*(10**6));
		PORT(	clk_in_50MHz: IN STD_LOGIC;
				clk_out		: BUFFER STD_LOGIC);
	END COMPONENT clock;

	COMPONENT sev_seg_drv
		PORT(	seven_input	: IN STD_LOGIC_VECTOR(3 DOWNTO 0);
				seven_output: OUT STD_LOGIC_VECTOR(6 DOWNTO 0));
	END COMPONENT sev_seg_drv;
	
	COMPONENT register16
	PORT(	datain		: IN STD_LOGIC_vector(15 DOWNTO 0);		
			enout16, enout8		: IN STD_LOGIC;				
			writein16, writein8	: IN STD_LOGIC;				
			dataout		: OUT STD_LOGIC_vector(15 DOWNTO 0));
	END COMPONENT register16;
	
	COMPONENT register32 
	PORT(	datain		: IN STD_LOGIC_vector(31 DOWNTO 0);		
			enout32,enout16,enout8	: IN STD_LOGIC;					
			writein32, writein16,writein8	: IN STD_LOGIC;					
			dataout	: OUT STD_LOGIC_vector(31 DOWNTO 0));
	END COMPONENT register32;
		
	COMPONENT Processor
	PORT(	instruction 	: IN  STD_LOGIC_VECTOR (31 DOWNTO 0);
			DataMemdatain 	: IN  STD_LOGIC_VECTOR (31 DOWNTO 0);
			DataMemdataout	: OUT STD_LOGIC_VECTOR (31 DOWNTO 0);
			DataMemAddr 	: OUT STD_LOGIC_VECTOR (31 DOWNTO 0);
			DataMemRead 	: OUT STD_LOGIC;
			DataMemWrite 	: OUT STD_LOGIC;
			clock 			: IN  STD_LOGIC);
	END COMPONENT Processor;
	
-- Begin logic for Mips_Proc
BEGIN
	ledG8 <= clk;
	
	sram_we <= sram_wein;
	sram_ce <= '0';
	sram_oe <= '0';
	sram_lb <= '0';
	sram_ub <= '0';
	output_1 <= sram_dq;
	ledR(15 DOWNTO 0) <= output_1;
	
	
-- port map components
	ck		: 	clock PORT MAP(clk_50, clk); -- system clock
	inst	:	register32 PORT MAP(instruction(31 DOWNTO 0), '0','0','0',inst_we,'0','0', inst32(31 DOWNTO 0)); -- instruction register
	proc	:	processor  PORT MAP(inst32(31 DOWNTO 0),dMDI(31 DOWNTO 0),dMDo(31 DOWNTO 0), -- processor
							    	dMAd(31 DOWNTO 0),dMRd, dMWr, procClk); -- processor
	-- seven segment display
	digit0 	: sev_seg_drv PORT MAP(displayVector(3 DOWNTO 0),sevD0);
	digit1	: sev_seg_drv PORT MAP(displayVector(7 DOWNTO 4),sevD1);
	digit2	: sev_seg_drv PORT MAP(displayVector(11 DOWNTO 8),sevD2);
	digit3	: sev_seg_drv PORT MAP(displayVector(15 DOWNTO 12),sevD3);
	digit4	: sev_seg_drv PORT MAP(displayVector(19 DOWNTO 16),sevD4);
	digit5	: sev_seg_drv PORT MAP(displayVector(23 DOWNTO 20),sevD5);
	digit6	: sev_seg_drv PORT MAP(displayVector(27 DOWNTO 24),sevD6);
	digit7	: sev_seg_drv PORT MAP(displayVector(31 DOWNTO 28),sevD7);			
									
	--sev_seg_drv switching -- 
WITH sw(17 DOWNTO 15) SELECT 
	displayVector <=  X"EEEE" & sram_dq WHEN "000",
	    address WHEN "001",
		 instruction WHEN "010",
		 dMDo WHEN "011",
		 dMAd WHEN "100",
		 dMDi WHEN "101",
		 X"EEE" & sram_addr WHEN "110",
		 X"EEE" & PC WHEN "111",
		 X"FFFFFFFF" WHEN OTHERS;

-- state machine
PROCESS(clk)
BEGIN
	IF	(clk'EVENT AND clk = '1' AND sw(0) = '1') THEN
	CASE current_state IS
	
	WHEN s0=> -- load program counter to address
		sram_addr <= PC;
		current_state <= s1;
		address <= X"EEEEEEE1";
		
	WHEN s1=> -- push lower half of instruction to register, increment sram address
		instruction(15 DOWNTO 0) <= sram_dq;
		sram_addr <= sram_addr + 1;					
		current_state <= s2;
		address <= X"EEEEEEE2";
		
	WHEN s2=> -- push upper half of instruction to register, increment PC, enable processor register write		
		instruction(31 DOWNTO 16) <= sram_dq;
		PC <= PC + 2;					
		inst_we <= '1';
		current_state <= s3;
		procClk <= '1';
		address <= X"EEEEEEE3";
		
	WHEN s3=> -- are we doing LW or SW op?
		inst_we <= '0';
		IF (dMRd = '1') THEN -- LW, read from SRAM
			sram_addr <= dMAd(19 DOWNTO 0);
		END IF;
		IF (dMWr = '1') THEN -- SW, write to SRAM
			sram_wein <= '1';
			sram_addr <= dMAd(19 DOWNTO 0);
		END IF;
		current_state <= s4;
		procClk <= '0';
		address <= X"EEEEEEE4";
		
	WHEN s4=>
		IF (dMRd = '1') THEN -- read from SRAM
			dMDi(15 DOWNTO 0) <= sram_dq;
			sram_addr <= sram_addr + 1;
		END IF;
		IF (dMWr = '1') THEN -- write to SRAM
			sram_dq <= dMDo(15 DOWNTO 0);
			sram_addr <= sram_addr + 1;				
		END IF;
		current_state <= s5;
		address <= X"EEEEEEE5";
		
	WHEN s5=>
		IF (dMRd = '1') THEN
			dMDi(31 DOWNTO 16) <= sram_dq;
		END IF;
		IF (dMWr = '1') THEN -- write to SRAM
			sram_dq <= dMDo(31 DOWNTO 16);
			sram_addr <= PC;
		END IF;
		current_state <= s6;
		address <= X"EEEEEEE6";
		
	WHEN s6=>
		IF (dMRd = '1') THEN
			dMDi <= data_SRAM;
			sram_addr <= PC;
		END IF;	
		current_state <= s0;
		address <= X"EEEEEEE0";
	END CASE;
	
END IF;
END PROCESS;
END behavior;

-----------------------------------------------------------------------------------

-----------------------------------------------------------------------------------
-- Begin Clock

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;
USE IEEE.STD_LOGIC_UNSIGNED.ALL;
USE IEEE.NUMERIC_STD.ALL;

ENTITY clock IS

GENERIC(n	: INTEGER := 25*(10**6));

 PORT(clk_in_50MHz	: IN STD_LOGIC;
      clk_out	: BUFFER STD_LOGIC := '0');
				
END clock;

ARCHITECTURE behavior OF clock IS

--SIGNAL ASSIGNMENTS

	--Creates a SIGNAL with a frequency of 1/(2n) of clock_in_50MHz
	--set n = 10**7 for an input clock frequency of 25MHz

    SIGNAL count		: 			integer range 0 to n;  --n IS half the period
	 
	BEGIN
	--Takes 50MHz clock input, reduces to 1/(2n) for system clock
	PROCESS (clk_in_50MHz)
		BEGIN
			IF (clk_in_50MHz'event AND clk_in_50MHz = '1') THEN
				count <= count + 1;
				IF (count = n-1) THEN
					clk_out <= NOT clk_out;  
					count <= 0; 
				END IF;
			END IF;
		END PROCESS;

END behavior;

-- End Clock

----------------------------------------------------------------------------

-- Begin seven segment driver

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;
USE IEEE.STD_LOGIC_UNSIGNED.ALL;
USE IEEE.NUMERIC_STD.ALL;


ENTITY sev_seg_drv IS 
	PORT(	-- The hex value to be displayed
			seven_input : IN STD_LOGIC_VECTOR(3 DOWNTO 0);     
			-- The seven segment display code.
			seven_output : OUT STD_LOGIC_VECTOR(6 DOWNTO 0));
END ENTITY sev_seg_drv;

ARCHITECTURE behavior OF sev_seg_drv IS
BEGIN

WITH seven_input SELECT seven_output <=
				"1000000" WHEN x"0",  -- '0'
				"1111001" WHEN x"1",  -- '1'
				"0100100" WHEN x"2",  -- '2'
				"0110000" WHEN x"3",  -- '3'
				"0011001" WHEN x"4",  -- '4' 
				"0010010" WHEN x"5",  -- '5'
            "0000010" WHEN x"6",  -- '6'
				"1111000" WHEN x"7",  -- '7'
				"0000000" WHEN x"8",  -- '8'
				"0011000" WHEN x"9",  -- '9'
				"0001000" WHEN x"A",  -- ‘A’
				"0000011" WHEN x"B",  -- ‘b’
				"1000110" WHEN x"C",  -- ‘C’
				"0100001" WHEN x"D",  -- ‘d’
				"0000110" WHEN x"E",  -- ‘E’
				"0001110" WHEN x"F";  -- ‘F’

END behavior;

-- End seven segment driver
-----------------------------------------------------------------------------

-- EOF