-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- UFPR, BCC, ci210 2014-2 trabalho semestral, autor: Roberto Hexsel, 27out
-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- curcuito que computa reducoes
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
use work.p_wires.all;

entity reduz is
  port (rst,rel : in    bit;
        valor   : in    bit_vector;
        saida   : out   bit_vector;
        sel     : out   bit);         -- active in '1'

  function BV2INT5(S: reg5) return integer is
    variable result: integer := 0;
  begin
    for i in 4 downto 0 loop
      result := result * 2;
      if S(i) = '1' then
        result := result + 1;
      end if;
    end loop;
    return result;
  end BV2INT5;

end reduz;

architecture functional of reduz is

  component ULA
    port (alfa,beta : in  bit_vector;
          oper      : in  bit_vector;
          delta     : out bit_vector);
  end component ULA;

  component registrador32 is
    port(rel, rst, ld: in  bit;
         D:           in  reg32;
         Q:           out reg32);
  end component registrador32;

  component registrador3 is
    port(rel, rst, ld: in  bit;
         D:           in  reg3;
         Q:           out reg3);
  end component registrador3;

  component count32dwn is
    port(rel, rst, ld, en: in  bit;
         D:               in  reg32;
         Q:               out reg32);
  end component count32dwn;

  type ctrl_record is record            -- palavra da memoria de controle
    PE    : reg3;                       -- proximo estado
    l_opr : bit;                        -- saida: carrega operacao
    clr_b : bit;                        -- saida: limpa resultado
    l_tam : bit;                        -- saida: carrega tamanho
    l_b   : bit;                        -- saida: carrega resultado
    en_d  : bit;                        -- saida: decrementa contador
    impr  : bit;                        -- saida: imprime resultado
  end record;
  type ctrl_memory is array (0 to 31) of ctrl_record;

  -- o indice desta memoria eh:  EA & sync & fim
  constant memoria : ctrl_memory := (
   -- PE   opr clr tam  b  decr impr
    -- idle = 0
    ("000",'0','0','0','0','0','0'),       -- 000 & 0=sync & fim=x
    ("000",'0','0','0','0','0','0'),       -- 000 & 0=sync & fim=x
    ("001",'0','0','0','0','0','0'),       -- 000 & 1=sync & fim=x
    ("001",'0','0','0','0','0','0'),       -- 000 & 1=sync & fim=x
    -- gravaopc = 1
    ("010",'1','0','0','0','0','0'),
    ("010",'1','0','0','0','0','0'),
    ("001",'1','0','0','0','0','0'),
    ("001",'1','0','0','0','0','0'),
    -- gravatam = 2
    ("011",'0','1','1','1','0','0'),
    ("011",'0','1','1','1','0','0'),
    ("011",'0','1','1','1','0','0'),
    ("011",'0','1','1','1','0','0'),
    -- gravaB = 3
    ("011",'0','0','0','1','1','0'),       -- 011 & 0=sync & fim=0
    ("100",'0','0','0','0','0','0'),       -- 011 & 0=sync & fim=1
    ("011",'0','0','0','1','1','0'),       -- 011 & 1=sync & fim=0
    ("100",'0','0','0','0','0','0'),       -- 011 & 1=sync & fim=1
    -- imprime = 4
    ("000",'0','0','0','0','0','1'),       -- 100 & 0=sync & fim=x
    ("000",'0','0','0','0','0','1'),       -- 100 & 0=sync & fim=x
    ("001",'0','0','0','0','0','1'),       -- 100 & 1=sync & fim=x
    ("001",'0','0','0','0','0','1'),       -- 100 & 1=sync & fim=x
    -- 5
    ("000",'0','0','0','0','0','0'),
    ("000",'0','0','0','0','0','0'),
    ("000",'0','0','0','0','0','0'),
    ("000",'0','0','0','0','0','0'),
    -- 6
    ("000",'0','0','0','0','0','0'),
    ("000",'0','0','0','0','0','0'),
    ("000",'0','0','0','0','0','0'),
    ("000",'0','0','0','0','0','0'),
    -- 7
    ("000",'0','0','0','0','0','0'),
    ("000",'0','0','0','0','0','0'),
    ("000",'0','0','0','0','0','0'),
    ("000",'0','0','0','0','0','0'));

  signal EA, PE : reg3;
  signal ender  : reg5;
  signal ctrl_word : ctrl_record;

  signal alfa,beta,delta, inp_beta, init, opcode, num : reg32;
  signal ld_alfa, ld_beta, ld_oper, clr_beta, ld_tam, en_dec, sync, fim : bit;
  signal ld_delta : bit;

begin  -- functional

  alfa <= valor;

  with opcode(2 downto 0) select
    init <=  x"00000000" when b"000", -- elemento neutro das operacoes
             x"ffffffff" when b"001",
             x"00000000" when b"010",
             x"00000000" when b"011",
             x"00000001" when b"100",
             x"00000000" when others;

  inp_beta <= delta when clr_beta = '0' else init;

  U_beta_reg: registrador32 port map(rel, rst, ld_beta, inp_beta, beta);

  U_ula: ULA port map(alfa, beta, opcode(2 downto 0), delta);

  U_delta_reg: registrador32 port map(rel, rst, '1', delta, saida);

  U_oper_reg: registrador32 port map(rel, rst, ld_oper, valor, opcode);

  U_tam_dec: count32dwn port map(rel, rst, ld_tam, en_dec, valor, num);

  sync <= '1' when valor = x"5555aaaa" else '0';

  fim  <= '1' when num = x"00000001" else '0';

  U_st_reg: registrador3 port map(rel, rst, '1', PE, EA);

  ender <= EA & sync & fim;  -- constroi endereco

  ctrl_word <= memoria( BV2INT5(ender) ); -- indice de vetor DEVE ser inteiro

  PE       <= ctrl_word.PE;            -- prox estado
  ld_oper  <= ctrl_word.l_opr;
  ld_tam   <= ctrl_word.l_tam;
  en_dec   <= ctrl_word.en_d;
  clr_beta <= ctrl_word.clr_b;
  ld_beta  <= ctrl_word.l_b;
  ld_delta <= ctrl_word.impr;

  sel <= ld_delta;

end functional;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- ULA
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use work.p_wires.all;

entity ULA is
  port (alfa,beta : in  reg32;
        oper      : in  reg3;
        delta     : out reg32);
end entity ULA;

architecture functional of ULA is
begin
  with oper select
    delta <= (alfa xor beta) when b"000",
             (alfa and beta) when b"001",
             (alfa or beta)  when b"010",
             INT2BV32(BV2INT(alfa) + BV2INT(beta)) when b"011",
             INT2BV32(BV2INT(alfa) * BV2INT(beta)) when b"100",
             x"00000000" when others;

end functional;
