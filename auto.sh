#!/bin/bash
nasm ./a.asm -o a.com
sudo umount /mnt/floppyB
sudo mount -o loop code.img /mnt/floppyB
sudo rm /mnt/floppyB/a.com
sudo cp ./a.com /mnt/floppyB
bochs