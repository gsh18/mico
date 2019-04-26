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
-- xnor de 32 bits, xnor bit a bit
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

use work.p_wires.all;

entity xnor32 is
  port(A,B : in reg32;
       Z   : out reg32);
end xnor32;

architecture behaviour of xnor32 is

  component inv is
    port(A : in bit;
         S : out bit);
  end component inv;

  component xor2 is
      port(A,B : in bit;
       S   : out bit);
  end component xor2;

  
  signal ot: reg32;
begin

  Uxor00: xor2 port map (A(0),B(0),ot(0));
  Uxor01: xor2 port map (A(1),B(1),ot(1));
  Uxor02: xor2 port map (A(2),B(2),ot(2));
  Uxor03: xor2 port map (A(3),B(3),ot(3));
  Uxor04: xor2 port map (A(4),B(4),ot(4));
  Uxor05: xor2 port map (A(5),B(5),ot(5));
  Uxor06: xor2 port map (A(6),B(6),ot(6));
  Uxor07: xor2 port map (A(7),B(7),ot(7));
  Uxor08: xor2 port map (A(8),B(8),ot(8));
  Uxor09: xor2 port map (A(9),B(9),ot(9));
  Uxor10: xor2 port map (A(10),B(10),ot(10));
  Uxor11: xor2 port map (A(11),B(11),ot(11));
  Uxor12: xor2 port map (A(12),B(12),ot(12));
  Uxor13: xor2 port map (A(13),B(13),ot(13));
  Uxor14: xor2 port map (A(14),B(14),ot(14));
  Uxor15: xor2 port map (A(15),B(15),ot(15));
  Uxor16: xor2 port map (A(16),B(16),ot(16));
  Uxor17: xor2 port map (A(17),B(17),ot(17));
  Uxor18: xor2 port map (A(18),B(18),ot(18));
  Uxor19: xor2 port map (A(19),B(19),ot(19));
  Uxor20: xor2 port map (A(20),B(20),ot(20));
  Uxor21: xor2 port map (A(21),B(21),ot(21));
  Uxor22: xor2 port map (A(22),B(22),ot(22));
  Uxor23: xor2 port map (A(23),B(23),ot(23));
  Uxor24: xor2 port map (A(24),B(24),ot(24));
  Uxor25: xor2 port map (A(25),B(25),ot(25));
  Uxor26: xor2 port map (A(26),B(26),ot(26));
  Uxor27: xor2 port map (A(27),B(27),ot(27));
  Uxor28: xor2 port map (A(28),B(28),ot(28));
  Uxor29: xor2 port map (A(29),B(29),ot(29));
  Uxor30: xor2 port map (A(30),B(30),ot(30));
  Uxor31: xor2 port map (A(31),B(31),ot(31));

  
  Unot00: inv port map (ot(0),Z(0));
  Unot01: inv port map (ot(1),Z(1));
  Unot02: inv port map (ot(2),Z(2));
  Unot03: inv port map (ot(3),Z(3));
  Unot04: inv port map (ot(4),Z(4));
  Unot05: inv port map (ot(5),Z(5));
  Unot06: inv port map (ot(6),Z(6));
  Unot07: inv port map (ot(7),Z(7));
  Unot08: inv port map (ot(8),Z(8));
  Unot09: inv port map (ot(9),Z(9));
  Unot10: inv port map (ot(10),Z(10));
  Unot11: inv port map (ot(11),Z(11));
  Unot12: inv port map (ot(12),Z(12));
  Unot13: inv port map (ot(13),Z(13));
  Unot14: inv port map (ot(14),Z(14));
  Unot15: inv port map (ot(15),Z(15));
  Unot16: inv port map (ot(16),Z(16));
  Unot17: inv port map (ot(17),Z(17));
  Unot18: inv port map (ot(18),Z(18));
  Unot19: inv port map (ot(19),Z(19));
  Unot20: inv port map (ot(20),Z(20));
  Unot21: inv port map (ot(21),Z(21));
  Unot22: inv port map (ot(22),Z(22));
  Unot23: inv port map (ot(23),Z(23));
  Unot24: inv port map (ot(24),Z(24));
  Unot25: inv port map (ot(25),Z(25));
  Unot26: inv port map (ot(26),Z(26));
  Unot27: inv port map (ot(27),Z(27));
  Unot28: inv port map (ot(28),Z(28));
  Unot29: inv port map (ot(29),Z(29));
  Unot30: inv port map (ot(30),Z(30));
  Unot31: inv port map (ot(31),Z(31));

end architecture behaviour;



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
-- mux de 2 entradas de 32 bits
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


library IEEE; use IEEE.std_logic_1164.all; use work.p_wires.all;

entity mux32x2 is
  port(A,B : in  reg32;
       S   : in  bit;
       Z   : out reg32);
end mux32x2;

architecture behaviour of mux32x2 is 

begin

  Z <= A when S = '1' else
       B;
      
end architecture behaviour;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- mux 4 entradas de 32 bits
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library IEEE; use IEEE.std_logic_1164.all; use work.p_wires.all;

entity mux32x4 is
  port(A,B,C,D  : in  reg32;
       S        : in  reg2;
       Z        : out reg32);
end mux32x4;

architecture behaviour of mux32x4 is 

begin

  Z <= A when S = "00" else
       B when S = "01" else
       C when S = "10" else
       D;
      
end architecture behaviour;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- mux 4 entradas de 16 bits
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library IEEE; use IEEE.std_logic_1164.all; use work.p_wires.all;

entity mux16x4 is
  port(A,B,C,D  : in  reg16;
       S        : in  reg2;
       Z        : out reg16);
end mux16x4;

architecture behaviour of mux16x4 is 

begin

  Z <= A when S = "00" else
       B when S = "01" else
       C when S = "10" else
       D;
      
end architecture behaviour;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- mux 2 entradas de 16 bits
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library IEEE; use IEEE.std_logic_1164.all; use work.p_wires.all;

entity mux16x2 is
  port(A,B      : in  reg16;
       S        : in  bit;
       Z        : out reg16);
end mux16x2;

architecture behaviour of mux16x2 is 

begin

  Z <= A when S = '0' else
       B;
      
end architecture behaviour;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- transforma uma entrada de 16 bits em 32 bits
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library IEEE; use IEEE.std_logic_1164.all; use work.p_wires.all;

entity extender is
  port(A        : in  reg16;
       Z        : out reg32);
end extender;

architecture behaviour of extender is 

begin


  -- extend immediate
  Z(15 downto 0) <= A; 
  Z(16) <= A(15);
  Z(17) <= A(15);
  Z(18) <= A(15);
  Z(19) <= A(15);
  Z(20) <= A(15);
  Z(21) <= A(15);
  Z(22) <= A(15);
  Z(23) <= A(15);
  Z(24) <= A(15);
  Z(25) <= A(15);
  Z(26) <= A(15);
  Z(27) <= A(15);
  Z(28) <= A(15);
  Z(29) <= A(15);
  Z(30) <= A(15);
  Z(31) <= A(15);

end architecture behaviour;

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
-- contador de 32 bits, reset=0 assincrono, load=1, enable=1 sincrono
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
      count <= x"0000";
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
      count <= x"00000000";
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
      value <= x"00000000";
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


-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- adderCadeia soma / subtrai 2 palavras de 32 bits
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

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
  U_b00: addBit port map ( inpA(0), inpB(0), vem,  r(0), v(0) );
  U_b01: addBit port map ( inpA(1), inpB(1), v(0), r(1), v(1) );
  U_b02: addBit port map ( inpA(2), inpB(2), v(1), r(2), v(2) );
  U_b03: addBit port map ( inpA(3), inpB(3), v(2), r(3), v(3) );
  U_b04: addBit port map ( inpA(4), inpB(4), v(3), r(4), v(4) );
  U_b05: addBit port map ( inpA(5), inpB(5), v(4), r(5), v(5) );
  U_b06: addBit port map ( inpA(6), inpB(6), v(5), r(6), v(6) );
  U_b07: addBit port map ( inpA(7), inpB(7), v(6), r(7), v(7) );
  U_b08: addBit port map ( inpA(8), inpB(8), v(7), r(8), v(8) );
  U_b09: addBit port map ( inpA(9), inpB(9), v(8), r(9), v(9) );
  U_b10: addBit port map ( inpA(10),inpB(10),v(9), r(10),v(10) );
  U_b11: addBit port map ( inpA(11),inpB(11),v(10),r(11),v(11) );
  U_b12: addBit port map ( inpA(12),inpB(12),v(11),r(12),v(12) );
  U_b13: addBit port map ( inpA(13),inpB(13),v(12),r(13),v(13) );
  U_b14: addBit port map ( inpA(14),inpB(14),v(13),r(14),v(14) );
  U_b15: addBit port map ( inpA(15),inpB(15),v(14),r(15),v(15) );
  U_b16: addBit port map ( inpA(16),inpB(16),v(15),r(16),v(16) );
  U_b17: addBit port map ( inpA(17),inpB(17),v(16),r(17),v(17) );
  U_b18: addBit port map ( inpA(18),inpB(18),v(17),r(18),v(18) );
  U_b19: addBit port map ( inpA(19),inpB(19),v(18),r(19),v(19) );
  U_b20: addBit port map ( inpA(20),inpB(20),v(19),r(20),v(20) );
  U_b21: addBit port map ( inpA(21),inpB(21),v(20),r(21),v(21) );
  U_b22: addBit port map ( inpA(22),inpB(22),v(21),r(22),v(22) );
  U_b23: addBit port map ( inpA(23),inpB(23),v(22),r(23),v(23) );
  U_b24: addBit port map ( inpA(24),inpB(24),v(23),r(24),v(24) );
  U_b25: addBit port map ( inpA(25),inpB(25),v(24),r(25),v(25) );
  U_b26: addBit port map ( inpA(26),inpB(26),v(25),r(26),v(26) );
  U_b27: addBit port map ( inpA(27),inpB(27),v(26),r(27),v(27) );
  U_b28: addBit port map ( inpA(28),inpB(28),v(27),r(28),v(28) );
  U_b29: addBit port map ( inpA(29),inpB(29),v(28),r(29),v(29) );
  U_b30: addBit port map ( inpA(30),inpB(30),v(29),r(30),v(30) );
  U_b31: addBit port map ( inpA(31),inpB(31),v(30),r(31),v(31) );
  
  vai <= v(31);
  outC <= r;
  
end adderCadeia;

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- soma / subtrai 2 palavras de 16 bits
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


library IEEE; use IEEE.std_logic_1164.all; use work.p_wires.all;

entity adderCadeia16 is
  port(inpA, inpB : in reg16;
       outC : out reg16;
       vem  : in bit;
       vai  : out bit
       );
end adderCadeia16;

architecture adderCadeia16 of adderCadeia16 is 
  component addBit port(bitA, bitB, vem : in bit;
                        soma, vai       : out bit);       
  end component addBit;

  signal v : reg16;                     -- cadeia de vai-um
  signal r : reg16;                     -- resultado parcial
begin

  -- entrada vem deve estar ligada em '0' para somar, em '1' para subtrair
  U_b00: addBit port map ( inpA(0), inpB(0), vem,  r(0), v(0) );
  U_b01: addBit port map ( inpA(1), inpB(1), v(0), r(1), v(1) );
  U_b02: addBit port map ( inpA(2), inpB(2), v(1), r(2), v(2) );
  U_b03: addBit port map ( inpA(3), inpB(3), v(2), r(3), v(3) );
  U_b04: addBit port map ( inpA(4), inpB(4), v(3), r(4), v(4) );
  U_b05: addBit port map ( inpA(5), inpB(5), v(4), r(5), v(5) );
  U_b06: addBit port map ( inpA(6), inpB(6), v(5), r(6), v(6) );
  U_b07: addBit port map ( inpA(7), inpB(7), v(6), r(7), v(7) );
  U_b08: addBit port map ( inpA(8), inpB(8), v(7), r(8), v(8) );
  U_b09: addBit port map ( inpA(9), inpB(9), v(8), r(9), v(9) );
  U_b10: addBit port map ( inpA(10),inpB(10),v(9), r(10),v(10) );
  U_b11: addBit port map ( inpA(11),inpB(11),v(10),r(11),v(11) );
  U_b12: addBit port map ( inpA(12),inpB(12),v(11),r(12),v(12) );
  U_b13: addBit port map ( inpA(13),inpB(13),v(12),r(13),v(13) );
  U_b14: addBit port map ( inpA(14),inpB(14),v(13),r(14),v(14) );
  U_b15: addBit port map ( inpA(15),inpB(15),v(14),r(15),v(15) );
  
  vai <= v(15);
  outC <= r;
  
end adderCadeia16;

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- multiplica por 1: A(15..0)*B(i) => S(16..0)
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE; use IEEE.std_logic_1164.all; use work.p_wires.all;

entity m_p_1 is
  port(A,B : in  reg32;                 -- entradas A,B
       S : in bit;                      -- bit por multiplicar
       R : out reg33);                  -- produto parcial
end m_p_1;

architecture funcional of m_p_1 is 
  component adderCadeia is
    port(inpA, inpB : in reg32;
         outC : out reg32;
         vem  : in bit;
         vai  : out bit
         );
  end component adderCadeia;

  signal somaAB : reg33;

begin

  U_soma: adderCadeia port map(A, B , somaAB(31 downto 0), '0', somaAB(32)); 

  R <= somaAB when S = '1' else ('0' & B);

end funcional;
-- -------------------------------------------------------------------


-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- multiplicador combinacional
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE; use IEEE.std_logic_1164.all; use IEEE.numeric_std.all;
use work.p_wires.all;

entity mult32x32 is
  port(A, B : in  reg32;   -- entradas A,B
       prod : out reg64);  -- produto
end mult32x32;

-- ------------------------------------------------------------------
-- multiplicador 16x16 adaptado para 32 bits
-- ------------------------------------------------------------------

architecture estrutural of mult32x32 is 
  component m_p_1 is
    port(A,B : in  reg32;   -- reg32
         S   : in  bit;
         R   : out reg33);  -- reg33
   end component m_p_1;
   
   signal p01,p02,p03,p04,p05,p06,p07,p08: reg33;
   signal p09,p10,p11,p12,p13,p14,p15,p16: reg33;
   signal p17,p18,p19,p20,p21,p22,p23,p24: reg33;
   signal p25,p26,p27,p28,p29,p30,p31,p32: reg33;
   
  begin
   U_00: m_p_1 port map (A,x"00000000",B(0),p01);
   U_01: m_p_1 port map (A,p01(32 downto 1),B(1),p02);
   U_02: m_p_1 port map (A,p02(32 downto 1),B(2),p03);
   U_03: m_p_1 port map (A,p03(32 downto 1),B(3),p04);
   U_04: m_p_1 port map (A,p04(32 downto 1),B(4),p05);
   U_05: m_p_1 port map (A,p05(32 downto 1),B(5),p06);
   U_06: m_p_1 port map (A,p06(32 downto 1),B(6),p07);
   U_07: m_p_1 port map (A,p07(32 downto 1),B(7),p08);
   U_08: m_p_1 port map (A,p08(32 downto 1),B(8),p09);
   U_09: m_p_1 port map (A,p09(32 downto 1),B(9),p10);
   U_10: m_p_1 port map (A,p10(32 downto 1),B(10),p11);
   U_11: m_p_1 port map (A,p11(32 downto 1),B(11),p12);
   U_12: m_p_1 port map (A,p12(32 downto 1),B(12),p13);
   U_13: m_p_1 port map (A,p13(32 downto 1),B(13),p14);
   U_14: m_p_1 port map (A,p14(32 downto 1),B(14),p15);
   U_15: m_p_1 port map (A,p15(32 downto 1),B(15),p16);

   U_16: m_p_1 port map (A,p16(32 downto 1),B(16),p17);
   U_17: m_p_1 port map (A,p17(32 downto 1),B(17),p18);
   U_18: m_p_1 port map (A,p18(32 downto 1),B(18),p19);
   U_19: m_p_1 port map (A,p19(32 downto 1),B(19),p20);
   U_20: m_p_1 port map (A,p20(32 downto 1),B(20),p21);
   U_21: m_p_1 port map (A,p21(32 downto 1),B(21),p22);
   U_22: m_p_1 port map (A,p22(32 downto 1),B(22),p23);
   U_23: m_p_1 port map (A,p23(32 downto 1),B(23),p24);
   U_24: m_p_1 port map (A,p24(32 downto 1),B(24),p25);
   U_25: m_p_1 port map (A,p25(32 downto 1),B(25),p26);
   U_26: m_p_1 port map (A,p26(32 downto 1),B(26),p27);
   U_27: m_p_1 port map (A,p27(32 downto 1),B(27),p28);
   U_28: m_p_1 port map (A,p28(32 downto 1),B(28),p29);
   U_29: m_p_1 port map (A,p29(32 downto 1),B(29),p30);
   U_30: m_p_1 port map (A,p30(32 downto 1),B(30),p31);
   U_31: m_p_1 port map (A,p31(32 downto 1),B(31),p32);


   prod <= p32 & p31(0) & p30(0) & p29(0) & p28(0) & p27(0) & p26(0) & p25(0) & p24(0) & p23(0) & p22(0) & p21(0) & p20(0) & p19(0) & p18(0) & p17(0) & p16(0) & p15(0) & p14(0) & p13(0) & p12(0) & p11(0) & p10(0) & p09(0) & p08(0) & p07(0) & p06(0) & p05(0) & p04(0) & p03(0) & p02(0) & p01(0);
 end estrutural;
-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

