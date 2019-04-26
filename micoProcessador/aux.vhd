-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- UFPR, BCC, ci210 2016-2 trabalho semestral, autor: Roberto Hexsel, 07out
-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


-- Acrescente modelos dos laboratorios a este arquivo


-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- inversor
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
use work.p_wires.all;

entity inv is
  generic (prop : time := t_inv);
  port(A : in bit;
       S : out bit);
end inv;

architecture comport of inv is 
begin
    S <= (not A) after prop;
end architecture comport;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- porta AND de 2 entradas
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
use work.p_wires.all;

entity and2 is
  generic (prop : time := t_and2);
  port(A, B : in  bit;  -- entradas A,B
       S    : out bit); -- saida C
end and2;

architecture and2 of and2 is 
begin
    S <= A and B after prop;
end and2;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- porta OR de 2 entradas
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
use work.p_wires.all;

entity or2 is
  generic (prop : time := t_or2);
  port(A,B : in bit;
       S   : out bit);
end or2;

architecture comport of or2 is 
begin
  S <= reject t_rej inertial (A or B) after prop;
end architecture comport;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- porta OR de 3 entradas
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
use work.p_wires.all;

entity or3 is
  generic (prop : time := t_or3);
  port(A, B, C : in  bit;  -- entradas A,B,C
       S       : out bit); -- saida S 
end or3;

architecture or3 of or3 is 
begin
    S <= A or B or C after prop;
end or3;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- porta XOR de 2 entradas
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
use work.p_wires.all;

entity xor2 is
  port(A,B : in bit;
       S   : out bit);
end xor2;

architecture comport of xor2 is 
begin
  S <= reject t_rej inertial (A xor B) after t_xor2;
end architecture comport;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- porta XOR de 3 entradas
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
use work.p_wires.all;

entity xor3 is
  generic (prop : time := t_xor3);
  port(A, B, C : in  bit;   -- entradas A,B,C
       S       : out bit);  -- saida S 
end xor3;

architecture xor3 of xor3 is 
begin
    S <= A xor B xor C after prop;
end xor3;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- mux2(a,b,s,z)
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
use work.p_wires.all;

entity mux2 is
  port(A,B : in  bit;
       S   : in  bit;
       Z   : out bit);
end mux2;

architecture estrut of mux2 is 
  component inv is
    generic (prop : time);
    port(A : in bit; S : out bit);
  end component inv;
  component and2 is
    generic (prop : time);
    port(A,B : in bit; S : out bit);
  end component and2;
  component or2 is
    generic (prop : time);
    port(A,B : in bit; S : out bit);
  end component or2;
  signal negs,f0,f1 : bit;
 begin

  Ui:  inv  generic map (t_inv)  port map(s,negs);
  Ua0: and2 generic map (t_and2) port map(a,negs,f0);
  Ua1: and2 generic map (t_and2) port map(b,s,f1);
  Uor: or2  generic map (t_or2)  port map(f0,f1,z);
    
end architecture estrut;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- mux32_2(a,b,s,z)
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE; use IEEE.std_logic_1164.all; use work.p_wires.all;

entity mux32_2 is
  port(a,b : in  reg32;                   -- entradas de dados
       s   : in  bit;                     -- entrada de selecao
       z   : out reg32);                  -- saida
end mux32_2;

architecture behaviour of mux32_2 is 

  
  signal result : reg32;
  signal R,P : reg32 ; 
  
begin  
	R <= a(31 downto 0);
	P <= b(31 downto 0);

	z <=  result(31 downto 0);
	
	result <=	R(31 downto 0) when s = '0' else
			P(31 downto 0) when s = '1' else
			x"00000000";

    
end behaviour;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- N-bit register, synchronous load active in '0', asynch reset
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE;
use work.p_WIRES.all;

entity registerN is
  generic (NUM_BITS: integer := 16;
           INIT_VAL: bit_vector);
  port(clk, rst, ld: in  bit;
       D:            in  bit_vector(NUM_BITS-1 downto 0);
       Q:            out bit_vector(NUM_BITS-1 downto 0));
end registerN;

architecture functional of registerN is
begin
  process(clk, rst, ld)
    variable state: bit_vector(NUM_BITS-1 downto 0);
  begin
    if rst = '0' then
      state := INIT_VAL;
    elsif rising_edge(clk) then
      if ld = '0' then
        state := D;
      end if;
    end if;
    Q <= state;
  end process;
  
end functional;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- contador de 16 bits, reset=0 assincrono, load=1, enable=1 sincrono
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library ieee; use ieee.std_logic_1164.all; use ieee.numeric_std.all;
use work.p_WIRES.all;

entity count16up is
  port(rel, rst, ld, en: in  bit;
        D:               in  reg16;
        Q:               out reg16);
end count16up;

architecture funcional of count16up is
  signal count: reg16;
begin

  process(rel, rst, ld)
    variable num : integer;
  begin
    if rst = '0' then
      count <=x"0000";
    elsif ld = '1' then
      count <= D;
    elsif en = '1' and rising_edge(rel) then
      num := BV2INT16(count) + 1;
      count <= INT2BV16(num);
    end if;
  end process;

  Q <= count after t_FFD;
end funcional;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- contador de 32 bits, reset=0 assincrono, load=1, enable=1 sincrono
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library ieee; use ieee.std_logic_1164.all; use ieee.numeric_std.all;
use work.p_WIRES.all;

entity count32dwn is
  port(rel, rst, ld, en: in  bit;
        D:               in  reg32;
        Q:               out reg32);
end count32dwn;

architecture funcional of count32dwn is
  signal count: reg32;
begin

  process(rel, rst, ld)
    variable num : integer;
  begin
    if rst = '0' then
      count <=x"00000000";
    elsif ld = '1' then
      count <= D;
    elsif en = '1' and rising_edge(rel) then
      num := BV2INT(count) - 1;
      count <= INT2BV32(num);
    end if;
  end process;

  Q <= count after t_FFD;
end funcional;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- registrador de 32 bits, reset=0 assincrono, load=1 sincrono
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
use work.p_WIRES.all;

entity registrador32 is
  port(rel, rst, ld: in  bit;
        D:           in  reg32;
        Q:           out reg32);
end registrador32;

architecture funcional of registrador32 is
  signal value: reg32;
begin

  process(rel, rst, ld)
  begin
    if rst = '0' then
      value <=x"00000000";
    elsif ld = '1' and rising_edge(rel) then
      value <= D;
    end if;
  end process;

  Q <= value after t_FFD;
end funcional;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- registrador de 20 bits, reset=0 assincrono, load=1 sincrono
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
use work.p_WIRES.all;

entity registrador20 is
  port(rel, rst, ld: in  bit;
        D:           in  reg20;
        Q:           out reg20);
end registrador20;

architecture funcional of registrador20 is
  signal value: reg20;
begin

  process(rel, rst, ld)
  begin
    if rst = '0' then
      value <= (others => '0');
    elsif ld = '1' and rising_edge(rel) then
      value <= D;
    end if;
  end process;

  Q <= value after t_FFD;
end funcional;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- flip-flop tipo D com set,reset=0 assincronos
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
use work.p_WIRES.all;
entity FFD is
  port(rel, rst, set : in bit;
        D : in  bit;
        Q : out bit);
end FFD;

architecture funcional of FFD is
  signal estado : bit := '0';
begin

  process(rel, rst, set)
  begin
    if rst = '0' then
      estado <= '0';
    elsif set = '0' then
      estado <= '1';
    elsif rising_edge(rel) then
      estado <= D;
    end if;
  end process;

  Q <= estado after t_FFD;

end funcional;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- flip-flop tipo D com set,reset=0 assincronos, saidas Q e /Q
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
use work.p_WIRES.all;
entity FFDQQ is
  port(rel, rst, set : in bit;
        D    : in  bit;
        Q, N : out bit);
end FFDQQ;

architecture funcional of FFDQQ is
  signal estado : bit := '0';
begin

  process(rel, rst, set)
  begin
    if rst = '0' then
      estado <= '0';
    elsif set = '0' then
      estado <= '1';
    elsif rising_edge(rel) then
      estado <= D;
    end if;
  end process;

  Q <= estado after t_FFD;
  N <= not estado after t_FFD;

end funcional;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- somador completo de um bit, modelo estrutural
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE; use IEEE.std_logic_1164.all; use work.p_wires.all;

entity addBit is
  port(bitA, bitB, vem : in bit;    -- entradas A,B,vem-um
       soma, vai       : out bit);  -- saida C,vai-um
end addBit;

architecture estrutural of addBit is 
  component and2 is generic (prop:time);
                      port (A,B: in bit; S: out bit);
  end component and2;

  component or3 is generic (prop:time);
                      port (A,B,C: in bit; S: out bit);
  end component or3;

  component xor3 is generic (prop:time);
                      port (A,B,C: in bit; S: out bit);
  end component xor3;

  signal a1,a2,a3: bit;
begin
  U_xor:  xor3 generic map ( t_xor3 ) port map ( bitA, bitB, vem, soma );

  U_and1: and2 generic map ( t_and2 ) port map ( bitA, bitB, a1 );
  U_and2: and2 generic map ( t_and2 ) port map ( bitA, vem,  a2 );
  U_and3: and2 generic map ( t_and2 ) port map ( vem,  bitB, a3 );
  U_or:   or3  generic map ( t_or3  ) port map ( a1, a2, a3, vai );

end estrutural;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- somador de 32 bits, sem adiantamento de vai-um
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE; use IEEE.std_logic_1164.all; use work.p_wires.all;

entity adderCadeia is 
  port(inpA, inpB : in reg32;
       outC : out reg32;
       vem  : in bit;
       vai  : out bit
       );
end adderCadeia;

architecture adderCadeia of adderCadeia is 
  component addBit port(bitA, bitB, vem : in bit;
                        soma, vai       : out bit);       
  end component addBit;

  signal v : reg32;                     -- cadeia de vai-um
  signal r : reg32;                     -- resultado parcial
begin

  -- entrada vem deve estar ligada em '0' para somar, em '1' para subtrair
  U_b0:  addBit port map ( inpA(0), inpB(0),  vem,   r(0),  v(0) );
  U_b1:  addBit port map ( inpA(1), inpB(1),  v(0),  r(1),  v(1) );
  U_b2:  addBit port map ( inpA(2), inpB(2),  v(1),  r(2),  v(2) );
  U_b3:  addBit port map ( inpA(3), inpB(3),  v(2),  r(3),  v(3) );
  U_b4:  addBit port map ( inpA(4), inpB(4),  v(3),  r(4),  v(4) );
  U_b5:  addBit port map ( inpA(5), inpB(5),  v(4),  r(5),  v(5) );
  U_b6:  addBit port map ( inpA(6), inpB(6),  v(5),  r(6),  v(6) );
  U_b7:  addBit port map ( inpA(7), inpB(7),  v(6),  r(7),  v(7) );
  U_b8:  addBit port map ( inpA(8), inpB(8),  v(7),  r(8),  v(8) );
  U_b9:  addBit port map ( inpA(9), inpB(9),  v(8),  r(9),  v(9) );
  U_ba:  addBit port map ( inpA(10),inpB(10), v(9),  r(10), v(10) );
  U_bb:  addBit port map ( inpA(11),inpB(11), v(10), r(11), v(11) );
  U_bc:  addBit port map ( inpA(12),inpB(12), v(11), r(12), v(12) );
  U_bd:  addBit port map ( inpA(13),inpB(13), v(12), r(13), v(13) );
  U_be:  addBit port map ( inpA(14),inpB(14), v(13), r(14), v(14) );
  U_bf:  addBit port map ( inpA(15),inpB(15), v(14), r(15), v(15) );
  U_b10: addBit port map ( inpA(16),inpB(16), v(15), r(16), v(16) );
  U_b11: addBit port map ( inpA(17),inpB(17), v(16), r(17), v(17) );
  U_b12: addBit port map ( inpA(18),inpB(18), v(17), r(18), v(18) );
  U_b13: addBit port map ( inpA(19),inpB(19), v(18), r(19), v(19) );
  U_b14: addBit port map ( inpA(20),inpB(20), v(19), r(20), v(20) );
  U_b15: addBit port map ( inpA(21),inpB(21), v(20), r(21), v(21) );
  U_b16: addBit port map ( inpA(22),inpB(22), v(21), r(22), v(22) );
  U_b17: addBit port map ( inpA(23),inpB(23), v(22), r(23), v(23) );
  U_b18: addBit port map ( inpA(24),inpB(24), v(23), r(24), v(24) );
  U_b19: addBit port map ( inpA(25),inpB(25), v(24), r(25), v(25) );
  U_b1a: addBit port map ( inpA(26),inpB(26), v(25), r(26), v(26) );
  U_b1b: addBit port map ( inpA(27),inpB(27), v(26), r(27), v(27) );
  U_b1c: addBit port map ( inpA(28),inpB(28), v(27), r(28), v(28) );
  U_b1d: addBit port map ( inpA(29),inpB(29), v(28), r(29), v(29) );
  U_b1e: addBit port map ( inpA(30),inpB(30), v(29), r(30), v(30) );
  U_b1f: addBit port map ( inpA(31),inpB(31), v(30), r(31), v(31) );
  vai <= v(31);
  outC <= r;
  
end adderCadeia;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- shift de um bit pra esquerda
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE; use IEEE.std_logic_1164.all; use work.p_wires.all;

entity shift_left1 is
  port(inp1 : in reg32;
 	cont: in bit;
       outp : out reg32
       );
end shift_left1;


architecture estrutural of shift_left1 is 

component mux2 is
  port(A,B : in  bit;
       S   : in  bit;
       Z   : out bit);
end component mux2;


  signal result : reg32;
  signal temp : reg32;

begin

outp(31 downto 0)<=result(31 downto 0);

temp(31 downto 0)<=inp1(31 downto 0);



Umux1:  mux2 port map(temp(0),  '0',    cont,result(0));
Umux2:  mux2 port map(temp(1),  temp(0), cont,result(1));
Umux3:  mux2 port map(temp(2),  temp(1), cont,result(2));
Umux4:  mux2 port map(temp(3),  temp(2), cont,result(3));
Umux5:  mux2 port map(temp(4),  temp(3), cont,result(4));
Umux6:  mux2 port map(temp(5),  temp(4), cont,result(5));
Umux7:  mux2 port map(temp(6),  temp(5), cont,result(6));
Umux8:  mux2 port map(temp(7),  temp(6), cont,result(7));
Umux9:  mux2 port map(temp(8),  temp(7), cont,result(8));
Umux10: mux2 port map(temp(9),  temp(8), cont,result(9));
Umux11: mux2 port map(temp(10), temp(9), cont,result(10));
Umux12: mux2 port map(temp(11), temp(10),cont,result(11));
Umux13: mux2 port map(temp(12), temp(11),cont,result(12));
Umux14: mux2 port map(temp(13), temp(12),cont,result(13));
Umux15: mux2 port map(temp(14), temp(13),cont,result(14));
Umux16: mux2 port map(temp(15), temp(14),cont,result(15));
Umux17: mux2 port map(temp(16), temp(15),cont,result(16));
Umux18: mux2 port map(temp(17), temp(16),cont,result(17));
Umux19: mux2 port map(temp(18), temp(17),cont,result(18));
Umux20: mux2 port map(temp(19), temp(18),cont,result(19));
Umux21: mux2 port map(temp(20), temp(19),cont,result(20));
Umux22: mux2 port map(temp(21), temp(20),cont,result(21));
Umux23: mux2 port map(temp(22), temp(21),cont,result(22));
Umux24: mux2 port map(temp(23), temp(22),cont,result(23));
Umux25: mux2 port map(temp(24), temp(23),cont,result(24));
Umux26: mux2 port map(temp(25), temp(24),cont,result(25));
Umux27: mux2 port map(temp(26), temp(25),cont,result(26));
Umux28: mux2 port map(temp(27), temp(26),cont,result(27));
Umux29: mux2 port map(temp(28), temp(27),cont,result(28));
Umux30: mux2 port map(temp(29), temp(28),cont,result(29));
Umux31: mux2 port map(temp(30), temp(29),cont,result(30));
Umux32: mux2 port map(temp(31), temp(30),cont,result(31));



end estrutural;
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- shift de um bit pra direita
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE; use IEEE.std_logic_1164.all; use work.p_wires.all;

entity shift_right1 is
  port(inp : in reg32;
	cont : in bit;
       outp : out reg32
       );
end shift_right1;

architecture estrutural of shift_right1 is 


component mux2 is
  port(A,B : in  bit;
       S   : in  bit;
       Z   : out bit);
end component mux2;


  signal result : reg32;
  signal temp : reg32;
  signal zero : bit;
begin


outp <= result;
temp<=inp;

zero <= '0';

Umux1:  mux2 port map(temp(0),  temp(1), cont,result(0));
Umux2:  mux2 port map(temp(1),  temp(2), cont,result(1));
Umux3:  mux2 port map(temp(2),  temp(3), cont,result(2));
Umux4:  mux2 port map(temp(3),  temp(4), cont,result(3));
Umux5:  mux2 port map(temp(4),  temp(5), cont,result(4));
Umux6:  mux2 port map(temp(5),  temp(6), cont,result(5));
Umux7:  mux2 port map(temp(6),  temp(7), cont,result(6));
Umux8:  mux2 port map(temp(7),  temp(8), cont,result(7));
Umux9:  mux2 port map(temp(8),  temp(9), cont,result(8));
Umux10: mux2 port map(temp(9),  temp(10),cont,result(9));
Umux11: mux2 port map(temp(10), temp(11),cont,result(10));
Umux12: mux2 port map(temp(11), temp(12),cont,result(11));
Umux13: mux2 port map(temp(12), temp(13),cont,result(12));
Umux14: mux2 port map(temp(13), temp(14),cont,result(13));
Umux15: mux2 port map(temp(14), temp(15),cont,result(14));
Umux16: mux2 port map(temp(15), temp(16),cont,result(15));
Umux17: mux2 port map(temp(16), temp(17),cont,result(16));
Umux18: mux2 port map(temp(17), temp(18),cont,result(17));
Umux19: mux2 port map(temp(18), temp(19),cont,result(18));
Umux20: mux2 port map(temp(19), temp(20),cont,result(19));
Umux21: mux2 port map(temp(20), temp(21),cont,result(20));
Umux22: mux2 port map(temp(21), temp(22),cont,result(21));
Umux23: mux2 port map(temp(22), temp(23),cont,result(22));
Umux24: mux2 port map(temp(23), temp(24),cont,result(23));
Umux25: mux2 port map(temp(24), temp(25),cont,result(24));
Umux26: mux2 port map(temp(25), temp(26),cont,result(25));
Umux27: mux2 port map(temp(26), temp(27),cont,result(26));
Umux28: mux2 port map(temp(27), temp(28),cont,result(27));
Umux29: mux2 port map(temp(28), temp(29),cont,result(28));
Umux30: mux2 port map(temp(29), temp(30),cont,result(29));
Umux31: mux2 port map(temp(30), temp(31),cont,result(30));
Umux32: mux2 port map(temp(31), zero    ,cont,result(31));

end architecture estrutural;

--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- shift de N bits pra esquerda
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE; use IEEE.std_logic_1164.all; use work.p_wires.all;

entity shift_leftN is
  port(inpn : in reg32;
	contn : in reg5;
       outpn : out reg32
       );
end shift_leftN;

architecture estrutural of shift_leftN is 

component shift_left1 is
  port(inp1 : in reg32;
	cont : in bit;
       outp : out reg32
       );
end component shift_left1;

signal sin1,  sin2,  sin3,  sin4,  sin5,  sin6,  sin7,  sin8,  sin9,  sin10,
       sin11, sin12, sin13, sin14, sin15, sin16, sin17, sin18, sin19, sin20, sin21,
       sin22, sin23, sin24, sin25, sin26, sin27, sin28, sin29, sin30, sin31 : reg32;


begin

 Ushif1:  shift_left1 port map(inpn,  contn(0),  sin1);
 Ushif2:  shift_left1 port map(sin1,  contn(1),  sin2);
 Ushif3:  shift_left1 port map(sin2,  contn(1),  sin3);
 Ushif4:  shift_left1 port map(sin3,  contn(2),  sin4);
 Ushif5:  shift_left1 port map(sin4,  contn(2),  sin5);
 Ushif6:  shift_left1 port map(sin5,  contn(2),  sin6);
 Ushif7:  shift_left1 port map(sin6,  contn(2),  sin7);
 Ushif8:  shift_left1 port map(sin7,  contn(3),  sin8);
 Ushif9:  shift_left1 port map(sin8,  contn(3),  sin9);
 Ushif10: shift_left1 port map(sin9,  contn(3), sin10);
 Ushif11: shift_left1 port map(sin10, contn(3), sin11);
 Ushif12: shift_left1 port map(sin11, contn(3), sin12);
 Ushif13: shift_left1 port map(sin12, contn(3), sin13);
 Ushif14: shift_left1 port map(sin13, contn(3), sin14);
 Ushif15: shift_left1 port map(sin14, contn(3), sin15);
 Ushif16: shift_left1 port map(sin15, contn(4), sin16);
 Ushif17: shift_left1 port map(sin16, contn(4), sin17);
 Ushif18: shift_left1 port map(sin17, contn(4), sin18);
 Ushif19: shift_left1 port map(sin18, contn(4), sin19);
 Ushif20: shift_left1 port map(sin19, contn(4), sin20);
 Ushif21: shift_left1 port map(sin20, contn(4), sin21);
 Ushif22: shift_left1 port map(sin21, contn(4), sin22);
 Ushif23: shift_left1 port map(sin22, contn(4), sin23);
 Ushif24: shift_left1 port map(sin23, contn(4), sin24);
 Ushif25: shift_left1 port map(sin24, contn(4), sin25);
 Ushif26: shift_left1 port map(sin25, contn(4), sin26);
 Ushif27: shift_left1 port map(sin26, contn(4), sin27);
 Ushif28: shift_left1 port map(sin27, contn(4), sin28);
 Ushif29: shift_left1 port map(sin28, contn(4), sin29);
 Ushif30: shift_left1 port map(sin29, contn(4), sin30);
 Ushif31: shift_left1 port map(sin30, contn(4), outpn);



end estrutural;
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- shift de N bits pra direita
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE; use IEEE.std_logic_1164.all; use work.p_wires.all;

entity shift_rightN is
  port(inpn : in reg32;
	contn : in reg5;
       outpn : out reg32
       );
end shift_rightN;


architecture estrutural of shift_rightN is 

component shift_right1 is
  port(inp : in reg32;
	cont : in bit;
       outp : out reg32
       );
end component shift_right1;

signal sin1,  sin2,  sin3,  sin4,  sin5,  sin6,  sin7,  sin8,  sin9,  sin10 : reg32;
signal sin11, sin12, sin13, sin14, sin15, sin16, sin17, sin18, sin19, sin20, sin21 : reg32;
signal sin22, sin23, sin24, sin25, sin26, sin27, sin28, sin29, sin30, sin31 : reg32;


begin

 Ushif1:  shift_right1 port map(inpn,  contn(0),  sin1);
 Ushif2:  shift_right1 port map(sin1,  contn(1),  sin2);
 Ushif3:  shift_right1 port map(sin2,  contn(1),  sin3);
 Ushif4:  shift_right1 port map(sin3,  contn(2),  sin4);
 Ushif5:  shift_right1 port map(sin4,  contn(2),  sin5);
 Ushif6:  shift_right1 port map(sin5,  contn(2),  sin6);
 Ushif7:  shift_right1 port map(sin6,  contn(2),  sin7);
 Ushif8:  shift_right1 port map(sin7,  contn(3),  sin8);
 Ushif9:  shift_right1 port map(sin8,  contn(3),  sin9);
 Ushif10: shift_right1 port map(sin9,  contn(3), sin10);
 Ushif11: shift_right1 port map(sin10, contn(3), sin11);
 Ushif12: shift_right1 port map(sin11, contn(3), sin12);
 Ushif13: shift_right1 port map(sin12, contn(3), sin13);
 Ushif14: shift_right1 port map(sin13, contn(3), sin14);
 Ushif15: shift_right1 port map(sin14, contn(3), sin15);
 Ushif16: shift_right1 port map(sin15, contn(4), sin16);
 Ushif17: shift_right1 port map(sin16, contn(4), sin17);
 Ushif18: shift_right1 port map(sin17, contn(4), sin18);
 Ushif19: shift_right1 port map(sin18, contn(4), sin19);
 Ushif20: shift_right1 port map(sin19, contn(4), sin20);
 Ushif21: shift_right1 port map(sin20, contn(4), sin21);
 Ushif22: shift_right1 port map(sin21, contn(4), sin22);
 Ushif23: shift_right1 port map(sin22, contn(4), sin23);
 Ushif24: shift_right1 port map(sin23, contn(4), sin24);
 Ushif25: shift_right1 port map(sin24, contn(4), sin25);
 Ushif26: shift_right1 port map(sin25, contn(4), sin26);
 Ushif27: shift_right1 port map(sin26, contn(4), sin27);
 Ushif28: shift_right1 port map(sin27, contn(4), sin28);
 Ushif29: shift_right1 port map(sin28, contn(4), sin29);
 Ushif30: shift_right1 port map(sin29, contn(4), sin30);
 Ushif31: shift_right1 port map(sin30, contn(4), outpn);




end estrutural;
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- multiplica por 1: A(15..0)*B(i) => S(16..0)
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE; use IEEE.std_logic_1164.all; use work.p_wires.all;

entity m_1 is
  port(A, B : in  reg32;                -- entradas A,B
       S : in bit;                      -- bit por multiplicar
       R : out reg33);                  -- produto parcial
end m_1;

architecture funcional of m_1 is 

component adderCadeia is
  port(inpA, inpB : in reg32;
       outC : out reg32;
       vem  : in  bit;    --https://www.linuxmint.com/start/sarah/
       vai  : out bit
       );
end component adderCadeia;

  signal somaAB : reg33;

begin

  U_soma: adderCadeia
    port map(A, B , somaAB(31 downto 0), '0', somaAB(32)); 

  -- defina a constante t_mux2 em packageWires.vhd
  R <= somaAB when S = '1' else ('0' & B) ;

end funcional;
-- -------------------------------------------------------------------


-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- multiplicador combinacional
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE; use IEEE.std_logic_1164.all; use IEEE.numeric_std.all;
use work.p_wires.all;

entity mult32_32 is
  port(A, B : in  reg32;   -- entradas A,B
       outp : out reg32);  -- produto
end mult32_32;


 architecture estrutural of mult32_32 is 
 
   component m_1 is port(A,B : in  bit_vector;   -- reg16
                           S   : in  bit;
                           R   : out bit_vector);  -- reg17
   end component m_1;
 
   signal p01,p02,p03,p04,p05,p06,p07,p08: reg33;
   signal p09,p10,p11,p12,p13,p14,p15,p16: reg33;
   signal p17,p18,p19,p20,p21,p22,p23,p24: reg33;
   signal p25,p26,p27,p28,p29,p30,p31,p32: reg33;
 
 begin
 
 U_00: m_1 port map (A, X"00000000", B(0) ,p01);
 U_01: m_1 port map (A, p01(32 downto 1), B(1), p02);
 U_02: m_1 port map (A, p02(32 downto 1), B(2), p03);
 U_03: m_1 port map (A, p03(32 downto 1), B(3), p04);
 U_04: m_1 port map (A, p04(32 downto 1), B(4), p05);
 U_05: m_1 port map (A, p05(32 downto 1), B(5), p06);
 U_06: m_1 port map (A, p06(32 downto 1), B(6), p07);
 U_07: m_1 port map (A, p07(32 downto 1), B(7), p08);
 U_08: m_1 port map (A, p08(32 downto 1), B(8), p09);
 U_09: m_1 port map (A, p09(32 downto 1), B(9), p10);
 U_10: m_1 port map (A, p10(32 downto 1), B(10), p11);
 U_11: m_1 port map (A, p11(32 downto 1), B(11), p12);
 U_12: m_1 port map (A, p12(32 downto 1), B(12), p13);
 U_13: m_1 port map (A, p13(32 downto 1), B(13), p14);
 U_14: m_1 port map (A, p14(32 downto 1), B(14), p15);
 U_15: m_1 port map (A, p15(32 downto 1), B(15), p16);
 U_16: m_1 port map (A, p16(32 downto 1), B(16), p17);
 U_17: m_1 port map (A, p17(32 downto 1), B(17), p18);
 U_18: m_1 port map (A, p18(32 downto 1), B(18), p19);
 U_19: m_1 port map (A, p19(32 downto 1), B(19), p20);
 U_20: m_1 port map (A, p20(32 downto 1), B(20), p21); 
 U_21: m_1 port map (A, p21(32 downto 1), B(21), p22);
 U_22: m_1 port map (A, p22(32 downto 1), B(22), p23);
 U_23: m_1 port map (A, p23(32 downto 1), B(23), p24);
 U_24: m_1 port map (A, p24(32 downto 1), B(24), p25);
 U_25: m_1 port map (A, p25(32 downto 1), B(25), p26);
 U_26: m_1 port map (A, p26(32 downto 1), B(26), p27);
 U_27: m_1 port map (A, p27(32 downto 1), B(27), p28);
 U_28: m_1 port map (A, p28(32 downto 1), B(28), p29);
 U_29: m_1 port map (A, p29(32 downto 1), B(29), p30);
 U_30: m_1 port map (A, p30(32 downto 1), B(30), p31);
 U_31: m_1 port map (A, p31(32 downto 1), B(31), p32);

 
 
 outp <=  p32(0) & p31(0) &  p30(0) & p29(0) &  P28(0) & p27(0) & p26(0) &  p25(0) & p24(0) & p23(0) & 
		  p22(0) & p21(0) &  p20(0) & p19(0) &  p18(0) & p17(0) & 
		  p16(0) & p15(0) &  p14(0) & p13(0) &  p12(0) & p11(0) &  p10(0) & p09(0) &
		  p08(0) & p07(0) &  p06(0) & p05(0) &  p04(0) & p03(0) &  p02(0) & p01(0);
   
 end estrutural;
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- or com constante lógica
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library IEEE; use IEEE.std_logic_1164.all; use work.p_wires.all;

entity ori_32 is
  port(inp1, inp2 : in reg32;
	outp : out reg32);
end ori_32;


architecture estrutural of ori_32 is

component or2 is
  generic (prop : time := t_or2);
  port(A,B : in bit;
       S   : out bit);
end component or2;

signal ent1 :reg32;
signal ent2 :reg32;
signal sai0 :reg32;

begin

ent1(31 downto 0)<=inp1(31 downto 0);
ent2(31 downto 0)<=inp2(31 downto 0);

outp(31 downto 0)<=sai0(31 downto 0);


Uor0: or2 port map( ent1(0),ent2(0), sai0(0));
Uor1: or2 port map( ent1(1),ent2(1), sai0(1));
Uor2: or2 port map( ent1(2),ent2(2), sai0(2));
Uor3: or2 port map( ent1(3),ent2(3), sai0(3));
Uor4: or2 port map( ent1(4),ent2(4), sai0(4));
Uor5: or2 port map( ent1(5),ent2(5), sai0(5));
Uor6: or2 port map( ent1(6),ent2(6), sai0(6));
Uor7: or2 port map( ent1(7),ent2(7), sai0(7));
Uor8: or2 port map( ent1(8),ent2(8), sai0(8));
Uor9: or2 port map( ent1(9),ent2(9), sai0(9));
Uor10: or2 port map( ent1(10),ent2(10), sai0(10));
Uor11: or2 port map( ent1(11),ent2(11), sai0(11));
Uor12: or2 port map( ent1(12),ent2(12), sai0(12));
Uor13: or2 port map( ent1(13),ent2(13), sai0(13));
Uor14: or2 port map( ent1(14),ent2(14), sai0(14));
Uor15: or2 port map( ent1(15),ent2(15), sai0(15));
Uor16: or2 port map( ent1(16),ent2(16), sai0(16));
Uor17: or2 port map( ent1(17),ent2(17), sai0(17));
Uor18: or2 port map( ent1(18),ent2(18), sai0(18));
Uor19: or2 port map( ent1(19),ent2(19), sai0(19));
Uor20: or2 port map( ent1(20),ent2(20), sai0(20));
Uor21: or2 port map( ent1(21),ent2(21), sai0(21));
Uor22: or2 port map( ent1(22),ent2(22), sai0(22));
Uor23: or2 port map( ent1(23),ent2(23), sai0(23));
Uor24: or2 port map( ent1(24),ent2(24), sai0(24));
Uor25: or2 port map( ent1(25),ent2(25), sai0(25));
Uor26: or2 port map( ent1(26),ent2(26), sai0(26));
Uor27: or2 port map( ent1(27),ent2(27), sai0(27));
Uor28: or2 port map( ent1(28),ent2(28), sai0(28));
Uor29: or2 port map( ent1(29),ent2(29), sai0(29));
Uor30: or2 port map( ent1(30),ent2(30), sai0(30));
Uor31: or2 port map( ent1(31),ent2(31), sai0(31));




end estrutural;

--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- mux2_32(a,b,s,z)
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
use work.p_wires.all;

entity mux2_32 is
  port(a,b : in  reg32;                   -- entradas de dados
       s   : in  bit;                   -- entrada de selecao
       z   : out reg32);                  -- saida
end mux2_32;

architecture estrut of mux2_32 is 
  
begin  -- compare ligacoes dos sinais com diagrama das portas logicas
  
z<= a when s ='0' else
    b when s='1';
    
end architecture estrut;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- mux4(a,b,c,d,s0,s1,z)
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
use work.p_wires.all;

entity mux4 is
  port(a,b,c,d : in  reg32;               -- quatro entradas de dados
       s0,s1   : in  bit;               -- dois sinais de selecao
       z       : out reg32);              -- saida
end mux4;

architecture estrut of mux4 is 

  component mux2_32 is
    port(A,B : in  reg32; S : in  bit; Z : out reg32);
  end component mux2_32;

  signal p,q : reg32;                     -- sinais internos
begin
  Um1: mux2_32 port map(a, b, s0, p);
  Um2: mux2_32 port map(c, d, s0, q);
  Umf: mux2_32 port map(p, q, s1, z);
  -- implemente usando tres mux2

end architecture estrut;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- mux8vet(entr(7downto0),sel(2downto0),z)
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
use work.p_wires.all;

entity mux8vet is
  port(entr0,entr1,entr2,entr3,entr4,entr5,entr6,entr7: in  reg32;
       sel  : in  reg3;
       z    : out reg32);
end mux8vet;

architecture estrut of mux8vet is 

  component mux2_32 is
    port(A,B : in  reg32; S : in  bit; Z : out reg32);
  end component mux2_32;

  component mux4 is
    port(A,B,C,D : in  reg32; S0,S1 : in  bit; Z : out reg32);
  end component mux4;

  signal x, y : reg32;
  
begin
  Um1: mux4 port map(entr0, entr1, entr2, entr3, sel(0), sel(1), x);
  Um2: mux4 port map(entr4, entr5, entr6, entr7, sel(0), sel(1), y);
  Umf: mux2_32 port map(x, y, sel(2), z);
  -- implemente usando dois mux4 e um mux2

end architecture estrut;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- mux16vet(entr(7downto0),sel(2downto0),z)
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
use work.p_wires.all;

entity mux16vet is
  port(entr0,entr1,entr2,entr3,entr4,entr5,entr6,entr7,entr8,entr9,entr10,entr11,entr12,entr13,entr14,entr15 : in  reg32;
       sel  : in  reg4;
       z    : out reg32);
end mux16vet;

architecture estrut of mux16vet is 

  component mux2_32 is
    port(A,B : in  reg32; S : in  bit; Z : out reg32);
  end component mux2_32;

component mux8vet is
  port(entr0,entr1,entr2,entr3,entr4,entr5,entr6,entr7 : in  reg32; sel  : in  reg3; z    : out reg32);
end component mux8vet;

  signal x, y : reg32;
  
begin
  Um1: mux8vet port map(entr0, entr1, entr2, entr3,entr4, entr5, entr6, entr7, sel(2 downto 0), x);
  Um2: mux8vet port map(entr8, entr9, entr10, entr11,entr12, entr13, entr14, entr15, sel(2 downto 0), y);
  Umf: mux2_32 port map(x, y, sel(3), z);


end architecture estrut;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


