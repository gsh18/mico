-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
--0;115;0c-- UFPR, BCC, ci210 2017-2 trabalho semestral, autor: Roberto Hexsel, 21out
-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- processador MICO XI
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library ieee; use ieee.std_logic_1164.all;
use IEEE.numeric_std.all;
use work.p_wires.all;


entity mico is
  port (rst,clk : in bit);
end mico;

architecture functional of mico is

  component mem_prog is                 -- no arquivo mem.vhd
    port (ender : in  reg7;
          instr : out reg32);
  end component mem_prog;

  component display is                  -- neste arquivo
    port (rst,clk : in bit;
          enable  : in bit;
          data    : in reg32);
  end component display;

  component ULA is                      -- neste arquivo
    port (fun : in reg4;
          alfa,beta : in  reg32;
          gama      : out reg32);
  end component ULA;
 
  component R is                        -- neste arquivo
    port (clk         : in  bit;
          wr_en       : in  bit;
          r_a,r_b,r_c : in  reg4;
          A,B         : out reg32;
          C           : in  reg32);
  end component R;

  component RAM is                      -- neste arquivo
    port (rst, clk : in  bit;
          sel      : in  bit;           -- ativo em 1
          wr       : in  bit;           -- ativo em 1
          ender    : in  reg16;
          data_inp : in  reg32;
          data_out : out reg32);
  end component RAM;
  
  component mux32x2 is                  -- aux.vhd
    port(A,B : in  reg32;               -- 2 entradas de 32 bits
         S   : in  bit;
         Z   : out reg32);
  end component mux32x2;

  component mux32x4 is                  -- aux.vhd
    port(A,B,C,D : in  reg32;           -- 4 entradas de 32 bits
         S       : in  reg2;
         Z       : out reg32);
  end component mux32x4;

  component mux16x4 is                  -- aux.vhd
    port(A,B,C,D : in  reg16;           -- 4 entradas de 16  bits
         S       : in  reg2;
         Z       : out reg16);
  end component mux16x4;

  component extender is                 -- aux.vhd
    port(A : in reg16;                  -- transforma sinal 16 bits em 32 bits
         Z : out reg32);
  end component extender;

  component counter is                 -- neste arquivo
  port(rel:              in  bit;
       S: in reg32;
       sel: in reg2;
       const: reg16;
        ip_in:           in  reg16;
        Q:               out reg16);
  end component counter;

  component xnor32 is                  -- aux.vhd
    port(A,B : in reg32;                -- verifica igualdade A e B
         Z   : out reg32);              -- bit a bit
  end component xnor32;
 
  type t_control_type is record
    selNxtIP   : reg2;     -- seleciona fonte do incremento do IP
    selC       : reg2;     -- seleciona fonte da escrita no reg destino
    wr_reg     : bit;      -- atualiza banco de registradores
    selBeta    : bit;      -- seleciona fonte para entrada B da ULA
    mem_sel    : bit;      -- habilita acesso a RAM
    mem_wr     : bit;      -- habilita escrita na RAM
    wr_display : bit;      -- atualiza display=1
  end record;

  type t_control_mem is array (0 to 15) of t_control_type;
  
  constant ctrl_table : t_control_mem := (
  --sNxtIP selC  wrR selB  Msel Mwr wrDsp
    ("01", "00", '0', '0', '0', '0', '0'),            -- NOP
    ("01", "00", '1', '0', '0', '0', '0'),            -- ADD
    ("01", "00", '1', '0', '0', '0', '0'),            -- SUB
    ("01", "00", '1', '0', '0', '0', '0'),            -- MUL
    ("01", "00", '1', '0', '0', '0', '0'),            -- AND
    ("01", "00", '1', '0', '0', '0', '0'),            -- OR
    ("01", "00", '1', '0', '0', '0', '0'),            -- XOR
    ("01", "00", '1', '0', '0', '0', '0'),            -- NOT
    ("01", "00", '1', '1', '0', '0', '0'),            -- ADDI
    ("01", "10", '1', '1', '1', '1', '0'),            -- LD
    ("01", "10", '0', '1', '1', '1', '0'),            -- ST
    ("01", "00", '0', '0', '0', '0', '1'),            -- SHOW
    ("11", "01", '1', '0', '0', '0', '0'),            -- JAL
    ("11", "00", '0', '0', '0', '0', '0'),            -- JR
    ("10", "00", '0', '0', '0', '0', '0'),            -- BRANCH
    ("00", "00", '0', '0', '0', '0', '0'));           -- HALT

  constant HALT : bit_vector := x"f";


  signal selNxtIP, selC : reg2;
  signal selBeta, wr_display, wr_reg : bit;
  signal mem_sel, mem_wr : bit;
  
  signal instr, A, B, C, S, beta, extended, ula_D, mem_D, ip_ext : reg32;
  signal this  : t_control_type;
  signal const, ext_zeros, ext_sinal, ip, ip_inc: reg16;
  signal opcode : reg4;
  signal i_opcode : natural range 0 to 15;
begin  -- functional

  -- memoria de programa contem somente 128 palavras
  U_mem_prog: mem_prog port map(ip(6 downto 0), instr); -- busca a instrucao no mem.vhd


  opcode <= instr(31 downto 28);        -- inidica a operacao a ser feita
  i_opcode <= BV2INT4(opcode);          -- indice do vetor DEVE ser inteiro
  const    <= instr(15 downto 0);       -- imediato
  this <= ctrl_table(i_opcode);         -- sinais de controle

  selBeta    <= this.selBeta;           -- em 1 seleciona imediato
  wr_display <= this.wr_display;        -- em 1 mostra no display o valor A
  selNxtIP   <= this.selNxtIP;          -- indica qual deve ser o IP
  wr_reg     <= this.wr_reg;            -- permite escrever no registro
  selC       <= this.selC;              -- permite selicionar a entrada do banco de reg
  mem_sel    <= this.mem_sel;           -- permite acessar a memoria
  mem_wr     <= this.mem_wr;            -- permite escrever na memoria

  -- transforma a constante da instrucao em sinal 32 bit
  U_ex0: extender port map (const,extended);

  -- transforma ip em 32 bits para guardar em um registrador
  U_ex1: extender port map (ip,ip_ext);

  -- seleciona qual entrada vai ser escrita no C do banco de reg
  U_mux1: mux32x4 port map (ula_D,ip_ext,mem_D,x"00000000",selC,C);

  -- Banco de registradores
  U_regs: R port map(clk,wr_reg,instr(27 downto 24),instr(23 downto 20),instr(19 downto 16),A,B,C);  


  -- IP counter
  U_IP: counter port map (clk,S,selNxtIP,const,ip,ip);

  -- mux para entrada 2 da ULA
  U_mux2: mux32x2 port map (extended,B,selBeta,beta);

  -- ULA
  U_ULA: ULA port map (opcode,A,beta,ula_D);

  -- RAM
  U_mem: RAM port map (rst, clk, mem_sel, mem_wr, ula_D(15 downto 0), B, mem_D);
         
  -- nao altere esta linha
  U_display: display port map (rst, clk, wr_display, A);

  -- mostra a linha em que se encontra o halt / parada de execução
  assert opcode /= HALT
    report LF & LF & "simulation halted: " & 
    "ender = "&integer'image(BV2INT16(ip))&" = "&BV16HEX(ip)&LF
    severity failure;
  
end functional;

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
--
-- Contador baseado no couterUP 32 bit do aux.vhd
--
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library ieee; use ieee.std_logic_1164.all; use ieee.numeric_std.all;
use work.p_WIRES.all;

entity counter is
  port(rel:              in  bit;
       S: in reg32;
       sel: in reg2;
       const: reg16;
        ip_in:           in  reg16;
        Q:               out reg16);
end counter;

architecture funcional of counter is
  signal ip_inc: reg16;
begin

  process(rel)
    variable num : integer;
  begin

    if rising_edge(rel) then

      case sel is
        when "10" => ip_inc <= const;
        when "11" => ip_inc <= const;
        when "01" => ip_inc <= ip_in;
        when "00" => ip_inc <= ip_in;

      num := BV2INT16(ip_inc) + 1; -- D + 1
      ip_inc <= INT2BV16(num); -- count = D
      end case;
    end if;
  end process;

  Q <= ip_inc; -- return count
end funcional;

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- ULA 
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
use work.p_wires.all;

entity ULA is
  port (fun : in reg4;
        alfa,beta : in  reg32;
        gama      : out reg32);
end ULA;

architecture behaviour of ULA is

  component adderCadeia is
    port(inpA, inpB : in reg32;
     outC : out reg32;
     vem  : in bit;
     vai  : out bit
     );
  end component adderCadeia;

  component mult32x32 is
    port(A, B : in  reg32;   -- entradas A,B
         prod : out reg64);  -- produto
  end component mult32x32;

  
-- signal
  
  signal r1,r2,r3,r4,r5,r6,r7,r8: reg32;
  signal r9: reg64;
begin  -- behaviour

  --    faz todas as operacoes sempre ao mesmo tempo
  --    mux no fim seleciona a saida correta
  
      Uadd0: adderCadeia port map(alfa,beta,r1,'0',open);     -- add
      Uadd1: adderCadeia port map(alfa,beta,r2,'1',open);     -- sub
      Umul0: mult32x32 	 port map(alfa,beta,r9);              -- mul
      Uand0: r3 <= (alfa(31 downto 0) and beta(31 downto 0)); -- and
      Uor00: r4 <= (alfa(31 downto 0) or beta(31 downto 0));  -- or 
      Uxor0: r5 <= (alfa(31 downto 0) xor beta(31 downto 0)); -- xor
      Unot0: r6 <= (not(alfa(31 downto 0)));                  -- not
      Uadd2: adderCadeia port map(alfa,beta,r7,'0',open);     -- ld
      Uadd3: adderCadeia port map(alfa,beta,r8,'0',open);     -- st
      

      -- mux que seleciona a saida correta
      
      gama <= r1 when fun = "0001" else
              r2 when fun = "0010" else
              r9(31 downto 0) when fun = "0011" else
              r3 when fun = "0100" else
              r4 when fun = "0101" else
              r5 when fun = "0110" else
              r6 when fun = "0111" else
              r1 when fun = "1000" else
              r7 when fun = "1001" else
              r8;
      
end behaviour;

-- -----------------------------------------------------------------------

      
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- display: exibe inteiro na saida padrao do simulador
--          NAO ALTERE ESTE MODELO
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE; use std.textio.all;
use work.p_wires.all;

entity display is
  port (rst,clk : in bit;
        enable  : in bit;
        data    : in reg32);
end display;

architecture functional of display is
  file output : text open write_mode is "STD_OUTPUT";
begin  -- functional

  U_WRITE_OUT: process(clk)
    variable msg : line;
  begin
    if falling_edge(clk) and enable = '1' then
      write ( msg, string'(BV32HEX(data)) );
      writeline( output, msg );
    end if;
  end process U_WRITE_OUT;

end functional;
-- ++ display ++++++++++++++++++++++++++++++++++++++++++++++++++++++++



-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- memoria RAM, com capacidade de 64K palavras de 32 bits
--          NAO ALTERE ESTE MODELO
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library ieee; use ieee.std_logic_1164.all;
use IEEE.numeric_std.all;
use work.p_wires.all;

entity RAM is
  port (rst, clk : in  bit;
        sel      : in  bit;          -- ativo em 1
        wr       : in  bit;          -- ativo em 1
        ender    : in  reg16;
        data_inp : in  reg32;
        data_out : out reg32);

  constant DATA_MEM_SZ : natural := 2**16;
  constant DATA_ADDRS_BITS : natural := log2_ceil(DATA_MEM_SZ);

end RAM;

architecture rtl of RAM is
  
  subtype t_address is unsigned((DATA_ADDRS_BITS - 1) downto 0);
  
  subtype word is bit_vector(31 downto 0);
  type storage_array is
    array (natural range 0 to (DATA_MEM_SZ - 1)) of word;
  signal storage : storage_array;
begin
  
  accessRAM: process(rst, clk, sel, wr, ender, data_inp)
    variable u_addr : t_address;
    variable index, latched : natural;

    variable d : reg32 := (others => '0');
    variable val, i : integer;

  begin

    if (rst = '0') and (sel = '1') then -- normal operation

      index := BV2INT16(ender);

      if  (wr = '1') and rising_edge(clk) then
        
        assert (index >= 0) and (index < DATA_MEM_SZ)
          report "ramWR index out of bounds: " & natural'image(index)
          severity failure;

        storage(index) <= data_inp;
        
        assert true report "ramWR["& natural'image(index) &"] "
          & BV32HEX(data_inp); -- DEBUG
        
      else

        assert (index >= 0) and (index < DATA_MEM_SZ)
          report "ramRD index out of bounds: " & natural'image(index)
          severity failure;

        d := storage(index);
        
        assert true report "ramRD["& natural'image(index) &"] "
          & BV32HEX(d);  -- DEBUG

      end if; -- normal operation

      data_out <= d;

    else

      data_out <= (others=>'0');

    end if; -- is reset?
    
  end process accessRAM; -- ---------------------------------------------
  
end rtl;
-- -----------------------------------------------------------------------



-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- banco de registradores
--          NAO ALTERE ESTE MODELO
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
use work.p_wires.all;

entity R is
  port (clk         : in  bit;
        wr_en       : in  bit;          -- ativo em 1
        r_a,r_b,r_c : in  reg4;
        A,B         : out reg32;
        C           : in  reg32);
end R;

architecture rtl of R is
  type reg_file is array(0 to 15) of reg32;
  signal reg_file_A : reg_file;
  signal reg_file_B : reg_file;
  signal int_ra, int_rb, int_rc : integer range 0 to 15;
begin

  int_ra <= BV2INT4(r_a);
  int_rb <= BV2INT4(r_b);
  int_rc <= BV2INT4(r_c);

  A <= reg_file_A( int_ra ) when r_a /= b"0000" else
       x"00000000";                        -- reg0 always zero
  B <= reg_file_B( int_rb ) when r_b /= b"0000" else
       x"00000000";

  WRITE_REG_BANKS: process(clk)
  begin
    if rising_edge(clk) then
      if wr_en = '1' and r_c /= b"0000" then
        reg_file_A( int_rc ) <= C;
        reg_file_B( int_rc ) <= C;
      end if;
    end if;
  end process WRITE_REG_BANKS;
  
end rtl;
-- -----------------------------------------------------------------------
