# Build Binaries (x86_64 Executable & Docker Image)

    make

## Runtime Dependencies

    $ ldd /usr/src/graph-check/dist/build/check_graph/check_graph
    linux-vdso.so.1 =>  (0x00007ffff8518000)
    libz.so.1 => /lib/x86_64-linux-gnu/libz.so.1 (0x00007f037f835000)
    librt.so.1 => /lib/x86_64-linux-gnu/librt.so.1 (0x00007f037f62d000)
    libdl.so.2 => /lib/x86_64-linux-gnu/libdl.so.2 (0x00007f037f428000)
    libpthread.so.0 => /lib/x86_64-linux-gnu/libpthread.so.0 (0x00007f037f20b000)
    libgmp.so.10 => /usr/lib/x86_64-linux-gnu/libgmp.so.10 (0x00007f037ef9d000)
    libm.so.6 => /lib/x86_64-linux-gnu/libm.so.6 (0x00007f037eca0000)
    libc.so.6 => /lib/x86_64-linux-gnu/libc.so.6 (0x00007f037e8e1000)
    /lib64/ld-linux-x86-64.so.2 (0x00007f037fa52000)

## Install Runtime Dependencies

    sudo apt-get install -y libgmp10
