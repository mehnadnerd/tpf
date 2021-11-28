#include <stdlib.h>
#include <iostream>
#include <verilated.h>
#include <verilated_vcd_c.h>
#include "VTIF.h"

#define MAX_SIM_TIME 20
vluint64_t sim_time = 0;

void step(VTIF *dut, VerilatedVcdC *m_trace, vluint64_t &sim_time) {
    dut->clock ^= 1;
    dut->eval();
    m_trace->dump(sim_time++);
    dut->clock ^= 1;
    dut->eval();
    m_trace->dump(sim_time++);
}

int main(int argc, char** argv, char** env) {
    VTIF *dut = new VTIF;

    Verilated::traceEverOn(true);
    VerilatedVcdC *m_trace = new VerilatedVcdC;
    dut->trace(m_trace, 5);
    m_trace->open("waveform.vcd");

    {
        dut->clock = 0;
        dut->reset = 1;
        dut->io_in_valid = 0;
        dut->io_in_data1 = 0;
        dut->io_in_data2 = 0;
        dut->io_in_done = 0;
        dut->eval();
        dut->clock = 1;
        dut->eval();
        step(dut, m_trace, sim_time);
        step(dut, m_trace, sim_time);
        dut->reset = 0;
        step(dut, m_trace, sim_time);
        step(dut, m_trace, sim_time);
    }


    dut->io_in_valid = 1;
    dut->io_in_data1 = 0x3f80;
    dut->io_in_data2 = 0x3f80;
    step(dut, m_trace, sim_time);

    dut->io_in_valid = 1;
    dut->io_in_data1 = 0x3f00;
    dut->io_in_data2 = 0x4000;
    step(dut, m_trace, sim_time);

    dut->io_in_valid = 1;
    dut->io_in_data1 = 0xbf80;
    dut->io_in_data2 = 0xbf80;
    step(dut, m_trace, sim_time);


    dut->io_in_valid = 1;
    dut->io_in_data1 = 0x0080;
    dut->io_in_data2 = 0x0080;
    step(dut, m_trace, sim_time);

    dut->io_in_valid = 1;
    dut->io_in_data1 = 0x0001;
    dut->io_in_data2 = 0x0001;
    step(dut, m_trace, sim_time);

    dut->io_in_valid = 1;
    dut->io_in_data1 = 0x7f7f;
    dut->io_in_data2 = 0x7f7f;
    step(dut, m_trace, sim_time);


    dut->io_in_valid = 0;
    step(dut, m_trace, sim_time);

    dut->io_in_done = 0;
    step(dut, m_trace, sim_time);
    step(dut, m_trace, sim_time);
    step(dut, m_trace, sim_time);
    step(dut, m_trace, sim_time);
    step(dut, m_trace, sim_time);
    step(dut, m_trace, sim_time);
    step(dut, m_trace, sim_time);
    step(dut, m_trace, sim_time);

    m_trace->dump(sim_time);
    m_trace->close();
    delete dut;
    exit(EXIT_SUCCESS);
}

