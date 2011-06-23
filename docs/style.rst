Coding Style
============


Linux style for indentation and braces.


I hate segfaults, so any exported function should check for NULL pointers.

For example::

    struct buffer *
    ncgr_get_pixels(struct NCGR *self)
    {
    	assert(self != NULL);
    	// ...
    }


