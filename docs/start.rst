
Overview
========

Spriterip is a collection of C routines for reading the Nitro files found on many DS games. It is capable of extracting images from the DS Pokémon games and converting them to PNG images.

The supported nitro formats are NARC, NCGR, NCLR, NCER, NANR, NMCR, and NMAR. Additionally, LZSS-compressed files are transparently decompressed.

There are two executables:

* ``rip``, a smallish C program which can extract *en masse* various types of sprites from Pokémon games
* ``ripscript``, which is a Scheme interpreter that exposes key Nitro-handling functions

No further developement is planned for ``rip``; many of its functions have already been rewritten as scripts.

Getting Started
===============

The first thing you might want to do is acquire the ``pokegra.narc`` file from a Pokémon game, because otherwise none of this is going to be very interesting.

``rip`` has no command-line interface; it just runs a function and exits. You can change the input file and destination dir through constants at the top of ``rip.c``

.. code-block:: c

    #define FILENAME "pokegra.narc"
    #define OUTDIR "test"

To change which function is run, look at the bottom of ``rip.c`` at the ``main()`` function. Uncomment the invocation of the function you wish to run and comment out the others.

Compile with ``make rip``. Run ``./rip``. Tada!
