
.. highlight:: c

C API
=====

Nitro
-----

.. c:function:: void *nitro_read(FILE *fp, off_t size)

    Read a known nitro file.

    ``size`` is used when reading an :ref:`LZSS`-compressed file. You may set it to 0 if
    the file you are loading isn't compressed.

    .. warning::

       It is *strongly* recommended that after calling this function, you check
       the type of the returned file to make sure that it is what you expected.

    ::

        struct NCGR *ncgr = nitro_read(fp, 0);
        if (ncgr == NULL) { goto error; }
        if (nitro_get_magic(ncgr) != NCGR_MAGIC) {
            u8 buf[MAGIC_BUF_SIZE];
            strmagic(ncgr, buf);
            fprintf(stderr, "Expected an NCGR, got %s", buf);
            goto error;
        }


NARC
----

.. c:function:: void *narc_load_file(struct NARC *self, int index)

   Read the file given by ``index`` in the NARC.
   Returns a newly-allocated nitro object, or ``NULL`` in the case of an error.


.. _lzss:

LZSS
----

LZSS is the compression algorithm used by the DS.
I have previously written about it on my blog: http://magikos.livejournal.com/7375.html.

On Linux, spriterip can transparently decompress files.
Windows is not supported because it does not support ``fmemopen(3)``.

There are a few things that could be done about this:

* The **boring** option is to rewrite lzss_decompress and the various fmt.read functions
  to read from memory (and write to memory).

* The **interesting** option is to write my own stream abstraction library.

* The **giving up** option is to rewrite spriterip in C++, or some other language with generic streams.

* The **cheating** option is to link against a different libc on Windows (instead of MSVCRT).
