cp ../TIF.v . \
&& verilator --trace --cc TIF.v --exe tb_tif.cc \
&& make -C obj_dir -f VTIF.mk VTIF \
&& ./obj_dir/VTIF
