#!/bin/bash
echo -e ",s/\$fwrite(32'h80000002,/\$display(/g\nw\nq" | ed TPF.v
scp TPF.v koopa:vlsi1/gradlab/TPF.v
