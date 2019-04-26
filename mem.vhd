-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- UFPR, BCC, ci210 2018-2 trabalho semestral, autor: Roberto Hexsel, 31out
-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

use work.p_wires.all;

entity mem_prog is
  port (ender : in  reg7;
        instr : out reg32);

  type t_prog_mem is array (0 to 127) of reg32;

  -- memoria de programa contem somente 64 palavras
  constant program : t_prog_mem := (
    x"00000000", -- nop                  0x00
    x"80010001", -- addi r1,r0,1         0x01   r1 <- 1
    x"b1000000", -- show r1              0x02
    x"8002fffe", -- addi r2,r0,-2        0x03   r2 <- -2
    x"b2000000", -- show r2              0x04
    x"11230000", -- add r3,r2,r1         0x05   r3 <- 1 + (-2)
    x"b3000000", -- show r3              0x06
    x"32340000", -- mul r4, r2, r3       0x07   r4 <- r2 * r3
    x"b4000000", -- show r5              0x08
    x"34250000", -- mul r5, r4, r2       0x09   r5 <- r4 * r2
    x"b5000000", -- show r5              0x0a
    x"00000000", -- nop                  0x0b
    x"80010008", --      r1 <= 8         0x0c   r1 <- zero + 8
    x"b1000000", --      show r1         0x0d
    x"80020001", --      r2 <= 1         0x0e   r2 <- zero + 1
    x"80030001", --      r3 <= 1         0x0f   r3 <- zero + 1
    x"e1000014", -- tst: r1 = r0 ? fim   0x10
    x"31220000", --      r2 <= r1 * r2   0x11
    x"8101ffff", --      r1 <= r1 - 1    0x12
    x"e0000010", --      bran tst        0x13
    x"b2000000", -- fim: show r2         0x14
    x"00000000", -- nop                  0x15
    x"800f4444", -- addi r15, r0, 0x4444 0x16   r15 <- zero OR 0x4444 
    x"a0f00100", -- st  r15, r0, 0x0100  0x17   M[ r1 + 0x100 ] r14 <- 0x4444
    x"bf000000", -- show  r15            0x18
    x"90080100", -- ld r8, r0, 0x0100    0x19  r8  <- M[ r0 + 0x100 ]
    x"88080001", -- addi r8, r8, 1       0xab  r8 <- r8 + 1
    x"b8000000", -- show  r8             0x1c
    x"00000000", -- nop                  0x1c
    x"00000000", -- nop                  0x1d
    x"00000000", -- nop                  0x1e
    x"f0000000", -- halt                 0x1f
    --x"00000000",
    --x"90040100", -- ld r4, 0x100(r0)	 - 0x00 - M[16][16]
    --x"80050000", -- addi r5, r0, 0	 - 0x01 - int r
    --x"c0000007", -- jal diag		 - 0x02	
    --x"c0000013", -- jal init		 - 0x03
    --x"87050000", -- addi r5,r7,0	 - 0x04- r = diag
    --x"b5000000", -- show r5		 - 0x05
    --x"f0000000", -- halt		 - 0x06
    --x"80060000", -- addi r6,r0,0	 - 0x07 - i = 0
    --x"80070000", -- addi r7,r0,0         - 0x08 - j = 0
    --x"80080000", -- addi r8,r0,16	 - 0x09
    --x"e6800012", --for1: bran r6,r8,exit - 0x0a - while i< 16
    --x"86060001", -- addi r6,r6,1	 - 0x0b - i++
    --x"e780000a", --for2: bran r7,r8,for1 - 0x0c - while j< 16
    --x"84040004", --addi r4,r4,4 	 - 0x0d - prox cel da mat
    --x"17690000", -- add r9,r6,r7	 - 0x0e - i + j
    --x"a4900004", -- st r9, 0(r4)	 - 0x0f - m[i][j] = i+j
    --x"87070001", --addi r7,r7,1		 - 0x10 - j++
    --x"e000000c", -- bran for2		 - 0x11
    --x"df000000", -- exit: jr r15	 - 0x12
    --x"80070000", --addi r7,r0,0 	 - 0x13 - s = 0
    --x"80060000", -- addi r6,r0,0        - 0x14 - i = 0 
    --x"90040100", --ld r4,0x100(r0)	 - 0x15-  m[0][0]
    --x"800a0011", --addi r10,r0, 17       - 0x16 - usa p chegar em m[i][i]
    --x"800b0004", --addi r11,r0,4	 - 0x17 - ""
    --x"3bab0000", --mul r11,r11,r10	 - 0x18 - 17*4
    --x"e680001d", --for3:bran r6,r8,exit  - 0x19 - while i< 16 
    --x"14770000", --addi r7,r7,r4         - 0x1a - s= s+ m[i][i]
    --x"86060000", --addi r6,r6,1		 - 0x1b - i++
    --x"ab440000", --add r4,r4,r11	 - 0x1c - vai pra m[i+1][i+1]
    --x"df000000", -- exit jr r15		 - 0x1d 
    --x"f0000000",

    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",

    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",

    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000",
    x"00000000"
  );


  function BV2INT7(S: reg7) return integer is
    variable result: integer;
  begin
    for i in S'range loop
      result := result * 2;
      if S(i) = '1' then
        result := result + 1;
      end if;
    end loop;
    return result;
  end BV2INT7;
  
end mem_prog;

-- nao altere esta arquitetura
architecture tabela of mem_prog is
begin  -- tabela

  instr <= program( BV2INT7(ender) );

end tabela;

