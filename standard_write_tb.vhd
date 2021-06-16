----------------------------------------------------------------------------------
-- Name: Daniel Hopfinger
-- Date: 08.06.2021
-- Module: standard_write_tb
-- Description: 
-- Testbench to i2c_master module. Plain write transaction is tested.
-- Testbench does 4 i2c transactions. The third transaction is a repeated start followed by a stop transaction.
-- The slave address and data is each time the same.
-- Master process starts transaction.
-- Slave process checks received data with sent data and generates always an ack.
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

library gen;

entity standard_write_tb is
end standard_write_tb;

architecture sim of standard_write_tb is

    constant C_CLK_PERIOD : time := 10 ns;
    constant C_I2C_PERIOD : time := 2500 ns;
    constant C_CONV_TIME_FREQ : integer := 1e9;

    constant C_SYS_CLK : integer := (1 * C_CONV_TIME_FREQ) / (integer(time'POS(C_CLK_PERIOD)) / 1000);
    constant C_I2C_CLK : integer := (1 * C_CONV_TIME_FREQ) / (integer(time'POS(C_I2C_PERIOD)) / 1000);

    constant C_NUM_OF_BYTES : integer := 20;

    constant C_INPUT_DATA_LENGTH : integer := 20;
    type data_arr is array (integer range 0 to C_INPUT_DATA_LENGTH) of std_logic_vector(7 downto 0);
    signal send_data : data_arr :=
    (x"A4", 
     x"00", x"01", x"02", x"03",
     x"04", x"05", x"06", x"07",
     x"08", x"09", x"0A", x"0B",
     x"0C", x"0D", x"0E", x"0F",
     x"10", x"11", x"12", x"13");
    

    signal clk          : std_logic := '1';
    signal rst          : std_logic := '1';
    signal scl          : std_logic;
    signal scl_out      : std_logic;
    signal scl_in       : std_logic := '1';
    signal scl_tri      : std_logic;
    signal sda          : std_logic;
    signal sda_out      : std_logic;
    signal sda_in       : std_logic := '1';
    signal sda_input    : std_logic := '1';
    signal sda_output   : std_logic;
    signal sda_tri      : std_logic;
    signal ack_error    : std_logic;
    signal en           : std_logic := '0';
    signal busy         : std_logic;
    signal stop_mode    : std_logic_vector(1 downto 0);
    signal wr_data      : std_logic_vector(7 downto 0);
    signal wr_en        : std_logic := '0';
    signal sending      : std_logic;
    signal rd_data      : std_logic_vector(7 downto 0);
    signal rd_en        : std_logic := '0';
    signal receiving    : std_logic;
    
    signal sending_r1 : std_logic;
    signal sending_rise_pulse : std_logic;
    signal sending_fall_pulse : std_logic;
    
    signal rec_data : std_logic_vector(7 downto 0);

begin

    clk <= not clk after C_CLK_PERIOD / 2;
    rst <= '0' after 10 * C_CLK_PERIOD;

    master_proc : process
    begin

        if rst = '1' then
            wait until rst = '0';
        end if;
        wait for 50 * C_CLK_PERIOD;
        wait until rising_edge(clk);

        for j in 0 to 3 loop
        
            en <= '1';

            if j = 2 then
                stop_mode <= "01";
            else
                stop_mode <= "00";
            end if;

            wait for 10 * C_CLK_PERIOD;

            wr_data <= send_data(0);
            wr_en <= '1';
            
            for i in 1 to C_INPUT_DATA_LENGTH loop
                wait until rising_edge(sending);
                wait for 1 * C_CLK_PERIOD;

                wr_data <= send_data(i);
                wr_en <= '1';

            end loop;

            wait until rising_edge(sending);
            wait for 1 * C_CLK_PERIOD;
            wr_data <= x"00";
            wr_en <= '0';
            wait until falling_edge(sending);
            wait for 1 * C_CLK_PERIOD;
            en <= '0';
            if stop_mode = "01" then
                wait for 1 * C_CLK_PERIOD;
                stop_mode <= "00";
                en <= '1';
            end if;

            if busy = '1' and stop_mode = "00" then
                wait until busy = '0';
            end if;
            wait for 1000 * C_CLK_PERIOD;
        
        end loop;

        std.env.stop(0);

    end process master_proc;

    i2c_master : entity gen.i2c_master(rtl)
    generic map (
        G_SYSTEM_CLOCK => C_SYS_CLK,
        G_BAUD_RATE    => C_I2C_CLK
    )
    port map (
        i_sys_clk   => clk,
        i_sys_rst   => rst,
        o_scl       => scl_out,
        i_scl       => scl_in,
        t_scl       => scl_tri,
        o_sda       => sda_out,
        i_sda       => sda_in,
        t_sda       => sda_tri,
        i_en        => en,
        o_busy      => busy,
        o_ack_error => ack_error,
        i_stop_mode => stop_mode,
        i_wr_data   => wr_data,
        i_wr_en     => wr_en,
        o_sending   => sending,
        o_rd_data   => rd_data,
        i_rd_en     => rd_en,
        o_receiving => receiving
    );
    scl <= scl_out when scl_tri = '0' else 'Z';
    scl_in <= scl when scl_tri = '0' else 'Z';

    sda <= sda_out when sda_tri = '0' else 'Z';
    sda_in <= sda when sda_tri = '0' else sda_input;

    slave_proc : process
    begin
        if rst = '1' then
            wait until rst = '0';
        end if;
        wait for 1 * C_CLK_PERIOD;
        wait until rising_edge(clk);

        rec_data <= x"00";

        for j in 0 to C_INPUT_DATA_LENGTH loop
            for i in 0 to 7 loop
                wait until rising_edge(scl);
                wait for C_I2C_PERIOD / 4;
                rec_data <= rec_data(6 downto 0) & sda;
            end loop;

            wait for C_I2C_PERIOD / 2;
            sda_input <= '0';

            assert rec_data = send_data(j)
            report "Data Error!" & lf &
                   "Expected: " & integer'image(to_integer(unsigned(send_data(j)))) & lf &
                   "Acutal: " & integer'image(to_integer(unsigned(rec_data)))
            severity failure;
            
            wait for 1 * C_CLK_PERIOD;
            wait for C_I2C_PERIOD;
            sda_input <= '1';
        
        end loop;
        
        if busy = '1' and stop_mode = "00" then
            wait until busy = '0';
        end if;
        if stop_mode = "01" then
            wait until rising_edge(scl);
        end if;

    end process slave_proc;

end sim;
