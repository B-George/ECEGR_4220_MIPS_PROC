--------------------------------------------------------------------------------
--
-- LAB #3
--
--------------------------------------------------------------------------------

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;
USE IEEE.STD_LOGIC_UNSIGNED.ALL;

ENTITY bitstorage IS

	PORT(bitin	: IN STD_LOGIC;
	     enout	: IN STD_LOGIC;
	     writein	: IN STD_LOGIC;
	     bitout	: OUT STD_LOGIC);

END ENTITY bitstorage;


ARCHITECTURE memlike OF bitstorage IS

	SIGNAL q: STD_LOGIC;

BEGIN
	PROCESS(writein) IS
	BEGIN
		IF (RISING_EDGE(writein)) THEN
			q <= bitin;
		END IF;
	END PROCESS;
	
	-- Note that data IS OUTput only WHEN enout = 0	
	bitout <= q WHEN enout = '0' ELSE 'Z';

END ARCHITECTURE memlike;

--------------------------------------------------------------------------------
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;
USE IEEE.STD_LOGIC_UNSIGNED.ALL;

ENTITY fulladder IS

    	PORT (a		: IN STD_LOGIC;
              b 	: IN STD_LOGIC;
              cin	: IN STD_LOGIC;
              sum 	: OUT STD_LOGIC;
              carry 	: OUT STD_LOGIC);

END fulladder;


ARCHITECTURE addlike OF fulladder IS

BEGIN
  	sum   <= a xor b xor cIN; 
  	carry <= (a AND b) OR (a AND cIN) OR (b AND cIN); 

END ARCHITECTURE addlike;


--------------------------------------------------------------------------------
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;
USE IEEE.STD_LOGIC_UNSIGNED.ALL;

ENTITY register8 IS

	PORT(datain	: IN STD_LOGIC_vector(7 DOWNTO 0);
	     enout	: IN STD_LOGIC;
	     writein	: IN STD_LOGIC;
	     dataout	: OUT STD_LOGIC_vector(7 DOWNTO 0));

END ENTITY register8;

ARCHITECTURE memmy OF register8 IS

	COMPONENT bitstorage
		PORT(bitin	: IN STD_LOGIC;
		     enout	: IN STD_LOGIC;
		     writein	: IN STD_LOGIC;
		     bitout	: OUT STD_LOGIC);
	END COMPONENT;

BEGIN
	
	R0: bitstorage PORT MAP(datain(0),enout,writein,dataout(0));
	R1: bitstorage PORT MAP(datain(1),enout,writein,dataout(1));
	R2: bitstorage PORT MAP(datain(2),enout,writein,dataout(2));
	R3: bitstorage PORT MAP(datain(3),enout,writein,dataout(3));
	R4: bitstorage PORT MAP(datain(4),enout,writein,dataout(4));
	R5: bitstorage PORT MAP(datain(5),enout,writein,dataout(5));
	R6: bitstorage PORT MAP(datain(6),enout,writein,dataout(6));
	R7: bitstorage PORT MAP(datain(7),enout,writein,dataout(7));
	
END ARCHITECTURE memmy;

--------------------------------------------------------------------------------
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;
USE IEEE.STD_LOGIC_UNSIGNED.ALL;

ENTITY register16 IS

	PORT(datain	: IN STD_LOGIC_vector(15 DOWNTO 0);
	     enout16,
 	     enout8	: IN STD_LOGIC;
	     writein16,
	     writein8	: IN STD_LOGIC;
	     dataout: OUT STD_LOGIC_vector(15 DOWNTO 0));

END ENTITY register16;

ARCHITECTURE biggermem OF register16 IS

	SIGNAL writeit8, writeit16: STD_LOGIC;
	SIGNAL enit8, enit16: STD_LOGIC;
	
	COMPONENT register8 IS
		PORT(datain: IN STD_LOGIC_vector(7 DOWNTO 0);
	 	    enout:  IN STD_LOGIC;
	 	    writein: IN STD_LOGIC;
	 	    dataout: OUT STD_LOGIC_vector(7 DOWNTO 0));
	END COMPONENT;

BEGIN
	writeit8 <= writein8 OR writein16;
	writeit16 <= writein16;
	

	enit8 <= enOUT8 AND enOUT16;
	enit16 <= enOUT16;

	Q0: register8 PORT MAP(datain(7 DOWNTO 0),enit8,writeit8,dataout(7 DOWNTO 0));
	Q1: register8 PORT MAP(datain(15 DOWNTO 8),enit16,writeit16,dataout(15 DOWNTO 8));
	
END ARCHITECTURE biggermem;
--------------------------------------------------------------------------------
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;
USE IEEE.STD_LOGIC_UNSIGNED.ALL;

ENTITY register32 IS
	PORT(datain: IN STD_LOGIC_vector(31 DOWNTO 0);
		 enOUT32,enOUT16,enOUT8: IN STD_LOGIC;
		 writein32, writein16, writein8: IN STD_LOGIC;
		 dataout: OUT STD_LOGIC_vector(31 DOWNTO 0));
END ENTITY register32;

ARCHITECTURE biggermem OF register32 IS

	SIGNAL writeit8, writeit16, writeit32: STD_LOGIC;
	SIGNAL enit8, enit16, enit32: STD_LOGIC;
	
	COMPONENT register8 IS
		PORT(datain: IN STD_LOGIC_vector(7 DOWNTO 0);
	 	    enout:  IN STD_LOGIC;
	 	    writein: IN STD_LOGIC;
	 	    dataout: OUT STD_LOGIC_vector(7 DOWNTO 0));
	END COMPONENT;

BEGIN
	writeit8 <= writein8 OR writein16 OR writein32;
	writeit16 <= writein16 OR writein32;
	writeit32 <= writein32;

	enit8 <= enOUT8 AND enOUT16 AND enOUT32;
	enit16 <= enOUT16 AND enOUT32;
	enit32 <= enOUT32;

	Q0: register8 PORT MAP(datain(7 DOWNTO 0),enit8,writeit8,dataout(7 DOWNTO 0));
	Q1: register8 PORT MAP(datain(15 DOWNTO 8),enit16,writeit16,dataout(15 DOWNTO 8));
	Q2: register8 PORT MAP(datain(23 DOWNTO 16),enit32,writeit32,dataout(23 DOWNTO 16));
	Q3: register8 PORT MAP(datain(31 DOWNTO 24),enit32,writeit32,dataout(31 DOWNTO 24));
	
END ARCHITECTURE biggermem;

--------------------------------------------------------------------------------
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;
USE IEEE.STD_LOGIC_UNSIGNED.ALL;

ENTITY adder_subtracter IS

	PORT(	datain_a: IN STD_LOGIC_vector(31 DOWNTO 0);
		datain_b: IN STD_LOGIC_vector(31 DOWNTO 0);
		add_sub	: IN STD_LOGIC; -- add when == '0'
		dataout	: OUT STD_LOGIC_vector(31 DOWNTO 0);
		co	: OUT STD_LOGIC);
		
END ENTITY adder_subtracter;

ARCHITECTURE calc OF adder_subtracter IS

	SIGNAL used_b: STD_LOGIC_vector(31 DOWNTO 0);
	SIGNAL carryit: STD_LOGIC_vector(31 DOWNTO 0);

	COMPONENT fulladder IS
    		PORT (a : IN STD_LOGIC;
          	      b : IN STD_LOGIC;
          	      cin : IN STD_LOGIC;
          	      sum : OUT STD_LOGIC;
         	      carry : OUT STD_LOGIC);
	END COMPONENT;

BEGIN
	used_b <= 	dataIN_b WHEN add_sub = '0' ELSE
			not(dataIN_b)+1;
	
	F1: fulladder PORT MAP(dataIN_a(0),used_b(0),'0',dataout(0),carryit(0));
	
	ADDVAL:	
	FOR i IN 1 to 31 GENERATE
		FA: fulladder PORT MAP(dataIN_a(i),used_b(i),carryit(i-1),dataout(i),carryit(i));
	END GENERATE ADDVAL;
		
	co <= carryit(31);

END ARCHITECTURE calc;

--------------------------------------------------------------------------------
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;
USE IEEE.STD_LOGIC_UNSIGNED.ALL;

ENTITY shift_register IS

	PORT(	datain: IN STD_LOGIC_vector(31 DOWNTO 0);
	   	dir: IN STD_LOGIC;
		shamt:	IN STD_LOGIC_vector(4 DOWNTO 0);
		dataout: OUT STD_LOGIC_vector(31 DOWNTO 0));
		
END ENTITY shift_register;

ARCHITECTURE shifter OF shift_register IS

	SIGNAL shammy: STD_LOGIC_vector(1 DOWNTO 0);
	
BEGIN
	shammy <= shamt(1 DOWNTO 0);

	dataout <= 	datain(30 DOWNTO 0) & "0" WHEN dir & shammy = "001" ELSE
			datain(29 DOWNTO 0) & "00" WHEN dir & shammy = "010" ELSE
			datain(28 DOWNTO 0) & "000" WHEN dir & shammy = "011" ELSE
			"0" & datain(31 DOWNTO 1) WHEN dir & shammy = "101" ELSE
			"00" & datain(31 DOWNTO 2) WHEN dir & shammy = "110" ELSE
			"000" & datain(31 DOWNTO 3) WHEN dir & shammy = "111" ELSE
			datain;
 
END ARCHITECTURE shifter;

------------------------------------------------------------------------------------

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;
USE IEEE.STD_LOGIC_UNSIGNED.ALL;

ENTITY ALU IS

	PORT(	datain1:   IN STD_LOGIC_vector(31 DOWNTO 0);
		datain2:   IN STD_LOGIC_vector(31 DOWNTO 0);
		Control:   IN STD_LOGIC_vector(4 DOWNTO 0);
		Zero:      OUT STD_LOGIC;
		ALUResult: OUT STD_LOGIC_vector(31 DOWNTO 0));
		
END ENTITY ALU;

ARCHITECTURE logic OF ALU IS

	--SIGNALS GO HERE
	SIGNAL excarry1: STD_LOGIC;
	SIGNAL excarry2: STD_LOGIC;

	SIGNAL addres: STD_LOGIC_vector(31 DOWNTO 0);
	SIGNAL subres: STD_LOGIC_vector(31 DOWNTO 0);
	SIGNAL orres: STD_LOGIC_vector(31 DOWNTO 0);
	SIGNAL andres: STD_LOGIC_vector(31 DOWNTO 0);
	SIGNAL sllres: STD_LOGIC_vector(31 DOWNTO 0);
	SIGNAL srlres: STD_LOGIC_vector(31 DOWNTO 0);

	SIGNAL result: STD_LOGIC_vector(31 DOWNTO 0);

	SIGNAL nores: STD_LOGIC_vector(31 DOWNTO 0):= (OTHERS=>'0');

	--COMPONENTS
	COMPONENT adder_subtracter IS
	PORT(	datain_a	: IN STD_LOGIC_vector(31 DOWNTO 0);
		datain_b	: IN STD_LOGIC_vector(31 DOWNTO 0);
		add_sub		: IN STD_LOGIC;
		dataout		: OUT STD_LOGIC_vector(31 DOWNTO 0);
		co		: OUT STD_LOGIC);
	END COMPONENT;

	COMPONENT shift_register IS
	PORT(	datain		: IN STD_LOGIC_vector(31 DOWNTO 0);
	   	dir		: IN STD_LOGIC;
		shamt		: IN STD_LOGIC_vector(4 DOWNTO 0);
		dataout		: OUT STD_LOGIC_vector(31 DOWNTO 0));
	END COMPONENT;
	
BEGIN
	
	A: adder_subtracter PORT MAP(datain1,datain2,'0',addres,excarry1);
	S: adder_subtracter PORT MAP(datain1,datain2,'1',subres,excarry2);	
	andres <= datain1 AND datain2; 
	orres <= datain1 OR datain2;
	L: shift_register PORT MAP(datain1,'0',datain2(4 DOWNTO 0),sllres);
	R: shift_register PORT MAP(datain1,'1',datain2(4 DOWNTO 0),srlres);
	
	WITH Control SELECT result <=
			addres WHEN "00000",
			subres WHEN "00001",
			andres WHEN "00010",
			orres WHEN "00011",
			sllres WHEN "00100",
			srlres WHEN "00101",
			nores WHEN OTHERS;

	WITH result SELECT Zero <=
			'1' WHEN x"00000000",
			'0' WHEN OTHERS;

	ALUResult <= result;

END ARCHITECTURE logic;

