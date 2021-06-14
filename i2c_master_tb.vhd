----------------------------------------------------------------------------------
-- Name: Daniel Hopfinger
-- Date: 08.06.2021
-- Module: i2c_master_tb
-- Description: 
-- Testbench to i2c_master module
-- 
-- History:
-- Version  | Date       | Information
-- ----------------------------------------
--  0.0.1   | 08.06.2021 | Initial version.
-- 
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_textio.all;

library std;
use std.textio.all;


entity i2c_master is
  port( 
    
  );
end i2c_master ;

architecture sim of i2c_master is

constant C_TIMEOUT_LIMIT : integer := 100;

type bus_sm_state is (bidle, wait_noop, wr_cmd, rd_cmd);
type busWidth is (BYTE, WORD, DWORD);

signal bus_state : bus_sm_state;
signal clk       : std_logic;

signal rdy : std_logic;
signal reg_addr : std_logic_vector(31 downto 0) := (others => '0');
signal writedata, read_data : std_logic_vector(31 downto 0) := (others => '0');
signal NIOS_ByteEn  : busWidth;

signal wait_cnt    : integer;
signal timeout_modifier : std_logic_vector(31 downto 0) := (others => '0');

signal lb_clk : std_logic;
signal lb_ena_clk : std_logic;
signal lb_bar : std_logic_vector(2 downto 0);
signal lb_addr : std_logic_vector(31 downto 0);
signal lb_wr_dat : std_logic_vector(31 downto 0);
signal lb_be : std_logic_vector(3 downto 0);
signal lb_rd : std_logic;
signal lb_wr : std_logic;

signal mmi_rd_data : std_logic_vector(31 downto 0) := (others=>'0');

signal line_index : integer := 0;
signal pos_index  : integer := 0;

signal cmd              : string(1 to 4) := "    ";
signal addr_32_tmp      : std_ulogic_vector(31 downto 0) := (others => '0');
signal data_32_tmp      : std_ulogic_vector(31 downto 0) := (others => '0');

signal wait_cycles      : integer := 0;

signal timeout_cnt      : integer := 0;

signal rd               : std_ulogic;
signal wr               : std_ulogic;
signal addr             : std_ulogic_vector(31 downto 0);
signal wr_data          : std_ulogic_vector(31 downto 0);

signal stop_sim         : std_ulogic := '0';

signal start_command    : std_ulogic := '0';    --handshake signals between 2 processes: a) proc_read_file  b) proc_gen_sig
signal finished_command : std_ulogic := '0';    --handshake signals between 2 processes: a) proc_read_file  b) proc_gen_sig

begin

mmi_clk_o     <= clk_i;
mmi_addr_o    <= addr;
mmi_wr_o      <= wr;
mmi_wr_be_o   <= "1111";
mmi_wr_data_o <= wr_data;
mmi_rd_o      <= rd;
mmi_rd_data   <= std_logic_vector(mmi_rd_data_i);

read_data <= mmi_rd_data xor timeout_modifier;
rdy       <= mmi_rdy_i;

proc_gen_sig: process(clk_i , reset_n)

  variable timeout_cnt : integer;

begin
  
  if reset_n = '0' then

    rd <= '0';
    wr <= '0';
        
  elsif rising_edge(clk_i) then
  
    finished_command <= '0';
    
    case bus_state is
      
      when bidle =>
        rd <= '0';
        wr <= '0';
        if start_command = '1' then
        
            if cmd="noop" or cmd = "NOOP" then
              bus_state <= wait_noop;
              wait_cnt  <= wait_cycles; 
            end if;
            
            if cmd="writ" or cmd = "WRIT" then
              bus_state <= wr_cmd;
              addr      <= addr_32_tmp;
              wr_data   <= data_32_tmp;
              wr        <= '1';
            end if;
            
            if cmd="read" or cmd = "READ" then
              bus_state <= rd_cmd;
              addr      <= addr_32_tmp;
              rd        <= '1';          
            end if;
            
        end if;
        timeout_cnt := 0;
        
      when wait_noop =>        
        wait_cnt <= wait_cnt - 1;
        if wait_cnt = 0 then
          bus_state <= bidle;
          finished_command <= '1';
        end if;
                        
      when wr_cmd =>
        if (rdy = '1') or (timeout_cnt > C_TIMEOUT_LIMIT) then
          wr <= '0';
          if rdy = '0' then
            report "TIMEOUT AT WRITE OPERATION" severity warning;
          end if;
          bus_state <= bidle;
          finished_command <= '1';
        end if;
        timeout_cnt := timeout_cnt + 1;
        
      when rd_cmd =>
          if (rdy = '1') or (timeout_cnt > C_TIMEOUT_LIMIT) then
            rd <= '0';
            if rdy = '1' then
              timeout_modifier <= (others => '0');
            else
              timeout_modifier <= (others => 'X');
              report "TIMEOUT AT READ OPERATION" severity warning;
            end if;
            bus_state <= bidle;
            finished_command <= '1';
          end if;
          timeout_cnt := timeout_cnt + 1;        
      
      when others => null;
        
    end case;
  end if;
  
end process proc_gen_sig;



proc_read_file: process

  file test_file : text is in G_TEST_FILE;  --"C:/CAX/Projects/innoroute/trust_node/source/verification_components/stim.txt";    
  variable test_line : line;    
  variable one_char  : character;
  variable neol      : boolean := true;-- Not End of Line        
  variable logic_representation : std_ulogic_vector(5 downto 0);
  variable is_hex_value : std_ulogic;
  variable is_dec_value : std_ulogic;
  variable hex_value    : std_ulogic_vector(3 downto 0);
  variable dec_value    : integer;
  variable n_read       : integer;   --number of data nibbles read within one line
  variable v_array_tmp  : std_ulogic_vector(67 downto 0);
  variable v_int_tmp    : integer;
  variable command      : string(1 to 4) := "####";
  variable noop_cycles  : integer;   
  
begin
      
  while not endfile(test_file) loop        --outer loop: each line within the file
    
    --wait for handshake signals being assigned       --just due to simulator
    wait for 10ps;

    if stop_sim = '1' then
      exit;
    end if;
        
    --before reading next line, check if previous command is finished, and reset a possible active start handshake
    if start_command = '1'and finished_command = '0' then
      wait until finished_command = '1';  --wait for incoming handshake being set
      start_command <= '0';               --reset outgoing handshake
      wait until finished_command = '0';  --wait for incoming handshake being reset
    end if;
   
    addr_32_tmp <= (others=>'0');
    data_32_tmp <= (others=>'0');
    cmd         <= "####";    
    
    --read one line from stimulifile
    readline (test_file, test_line);
    
    --increment line counter
    line_index <= line_index + 1;  
          
    --check if the line contains enough characters
    if test_line'length < 6 then   -- minimum size is 6:  4char cmd  +  1 char blank + 1 char        e.g. noop 1 
      next;     --skip to next line
    end if;
    
    --read first 4 chars: should be valid command
    read(test_line, one_char, neol);
      command(1) := one_char;
      --if one_char = '#' then next; end if;
    read(test_line, one_char, neol);
      command(2) := one_char;
    read(test_line, one_char, neol);
      command(3) := one_char;
    read(test_line, one_char, neol);
      command(4) := one_char;

    --now that we have read 4 chars = cmd
    cmd <= command;

    --check only for supported commands, simply ignore all other stuff
    case command is
      when "noop" | "NOOP" =>
        noop_cycles := 0;
        --read 1 char blank + x chars noops until eol or blank char
        read(test_line, one_char, neol); 
        
        loop
          read(test_line, one_char, neol);
          if (neol = false) or (one_char=' ') then         --END OF LINE or BLANK ?
            exit;
          end if;       
          logic_representation := char_to_hex(one_char);
          if logic_representation(4) = '1' then   --if char represents decimal digit
            noop_cycles := noop_cycles * 10 + To_integer(unsigned(logic_representation(3 downto 0)));
          end if;
        end loop;
                     
        if noop_cycles > 0 then
          wait_cycles <= noop_cycles;
          start_command <= '1';
        end if;
        
      when "writ" | "WRIT" =>
        --read 1 char blank + 8 chars addr + 1 blank + 8 chars data
        read(test_line, one_char, neol);
        read(test_line, one_char, neol); logic_representation := char_to_hex(one_char); addr_32_tmp(31 downto 28) <= logic_representation(3 downto 0);
        read(test_line, one_char, neol); logic_representation := char_to_hex(one_char); addr_32_tmp(27 downto 24) <= logic_representation(3 downto 0);
        read(test_line, one_char, neol); logic_representation := char_to_hex(one_char); addr_32_tmp(23 downto 20) <= logic_representation(3 downto 0);
        read(test_line, one_char, neol); logic_representation := char_to_hex(one_char); addr_32_tmp(19 downto 16) <= logic_representation(3 downto 0);
        read(test_line, one_char, neol); logic_representation := char_to_hex(one_char); addr_32_tmp(15 downto 12) <= logic_representation(3 downto 0);
        read(test_line, one_char, neol); logic_representation := char_to_hex(one_char); addr_32_tmp(11 downto  8) <= logic_representation(3 downto 0);
        read(test_line, one_char, neol); logic_representation := char_to_hex(one_char); addr_32_tmp( 7 downto  4) <= logic_representation(3 downto 0);
        read(test_line, one_char, neol); logic_representation := char_to_hex(one_char); addr_32_tmp( 3 downto  0) <= logic_representation(3 downto 0);
        read(test_line, one_char, neol);
        read(test_line, one_char, neol); logic_representation := char_to_hex(one_char); data_32_tmp(31 downto 28) <= logic_representation(3 downto 0);
        read(test_line, one_char, neol); logic_representation := char_to_hex(one_char); data_32_tmp(27 downto 24) <= logic_representation(3 downto 0);
        read(test_line, one_char, neol); logic_representation := char_to_hex(one_char); data_32_tmp(23 downto 20) <= logic_representation(3 downto 0);
        read(test_line, one_char, neol); logic_representation := char_to_hex(one_char); data_32_tmp(19 downto 16) <= logic_representation(3 downto 0);
        read(test_line, one_char, neol); logic_representation := char_to_hex(one_char); data_32_tmp(15 downto 12) <= logic_representation(3 downto 0);
        read(test_line, one_char, neol); logic_representation := char_to_hex(one_char); data_32_tmp(11 downto  8) <= logic_representation(3 downto 0);
        read(test_line, one_char, neol); logic_representation := char_to_hex(one_char); data_32_tmp( 7 downto  4) <= logic_representation(3 downto 0);
        read(test_line, one_char, neol); logic_representation := char_to_hex(one_char); data_32_tmp( 3 downto  0) <= logic_representation(3 downto 0);          
        start_command <= '1';
                    
      when "read" | "READ" =>
        --read 1 char blank + 8 chars addr
        read(test_line, one_char, neol);
        read(test_line, one_char, neol); logic_representation := char_to_hex(one_char); addr_32_tmp(31 downto 28) <= logic_representation(3 downto 0);
        read(test_line, one_char, neol); logic_representation := char_to_hex(one_char); addr_32_tmp(27 downto 24) <= logic_representation(3 downto 0);
        read(test_line, one_char, neol); logic_representation := char_to_hex(one_char); addr_32_tmp(23 downto 20) <= logic_representation(3 downto 0);
        read(test_line, one_char, neol); logic_representation := char_to_hex(one_char); addr_32_tmp(19 downto 16) <= logic_representation(3 downto 0);
        read(test_line, one_char, neol); logic_representation := char_to_hex(one_char); addr_32_tmp(15 downto 12) <= logic_representation(3 downto 0);
        read(test_line, one_char, neol); logic_representation := char_to_hex(one_char); addr_32_tmp(11 downto  8) <= logic_representation(3 downto 0);
        read(test_line, one_char, neol); logic_representation := char_to_hex(one_char); addr_32_tmp( 7 downto  4) <= logic_representation(3 downto 0);
        read(test_line, one_char, neol); logic_representation := char_to_hex(one_char); addr_32_tmp( 3 downto  0) <= logic_representation(3 downto 0);
        start_command <= '1';

      when "exit" | "EXIT" =>
        --stop reading stimuli file
        stop_sim <= '1';        
                  
      when others=>     --in case of unsupported command skip to next line
         next;          --next loop
    end case;
    
    
    --if eol is not yet reached, then read in loop until eol  
    if neol = false then  --END OF LINE ?
      next;
    end if;    
    loop                  --loop until end of line
      read(test_line, one_char, neol);
      if neol = false then         --END OF LINE ?
        exit;
      end if;
    end loop;

    wait for 10ps;
          
  end loop;
  
  start_command <= '0';
  
  report "REACHED END OF MMI MASTER STIMULIFILE" severity warning;
  
  wait;  --forever
    
end process proc_read_file;


end sim;
