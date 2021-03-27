@echo off

set name="balonfgt_prg_8000"

ca65 %name%.asm -g
rem ca65 balonfgt_vectors.asm -g

ld65 -C cnrom_32k_horz.cfg -o %name%.nes %name%.o -Ln labels.txt || goto fail


del *.o

mesen %name%.nes