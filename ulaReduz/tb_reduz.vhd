-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- UFPR, BCC, ci210 2014-2 trabalho semestral, autor: Roberto Hexsel, 26out
-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- testbench para circuito para redução de vetores
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library ieee; use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.p_wires.all;

entity tb_reduz is
end tb_reduz;

architecture TB of tb_reduz is

  component read_data_file is
    generic (INPUT_FILE_NAME : string := "input.data");
    port (rst,rel : in    bit;
          data  : out   bit_vector;
          eof   : out   bit);
  end component read_data_file;

  component write_int is
    port (rst,rel : in    bit;
          sel   : in    bit;         -- active in '1'
          data  : in    bit_vector);
  end component write_int;

  component reduz is
    port (rst,rel : in    bit;
          valor   : in    bit_vector;
          saida   : out   bit_vector;
          sel     : out   bit);         -- active in '1';
  end component reduz;

  signal rel,rst : bit;
  signal r_nxt, r_rdy, r_eof, w_sel, w_rdy: bit;
  signal entrada, saida : reg32;

begin  -- TB

  U_read_input: read_data_file generic map ("input.data")
    port map (rst,rel, entrada, r_eof);

  U_reduz: reduz port map (rst,rel, entrada, saida, w_sel);

  U_write_output:  write_int port map (rst,rel, w_sel, saida);

  
  U_clock: process
  begin
    rel <= '1';      -- executa e
    wait for t_clock_period / 2;  -- espera meio ciclo
    rel <= '0';      -- volta a executar e
    wait for t_clock_period / 2;  -- espera meio ciclo e volta ao topo
  end process;

  U_reset: process
  begin
    rst <= '0';      -- executa e
    wait for t_clock_period * 0.25;  -- espera por 1/4 de ciclo
    rst <= '1';      -- volta a executar e
    wait;            -- espera para sempre
  end process;

end TB;
-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

----------------------------------------------------------------
configuration CFG_TB of tb_reduz is
	for TB
        end for;
end CFG_TB;
----------------------------------------------------------------
