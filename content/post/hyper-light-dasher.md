[[!meta title="Automating Hyper Light Drifter's 800 Dash Challenge on the Nintendo Switch"]]
[[!meta date="2018-10-04T10:30:00-05:00"]]

I do all of my gaming these days on a Nintendo Switch (with the exception of
[Zachtronics][1] games on my Linux machine).  Lately I've been playing [Hyper
Light Drifter][2], a gorgeous retro-styled action adventure game that its
creator calls a mix of 2D Zelda and Diablo.  The game is challenging, but plays
nearly perfectly. Except...

There is one optional challenge which calls for you to dash 800 times
consecutively in a confined space. This requires you to button mash with
precise timing while directing the character in a circle to avoid bumping into
any walls. It's brutal, and [discussion spaces][2] related to the game are
filled with stories and complaints about this challenge.

After many failed attempts and probably tens of thousands of button mashes
later, I decided there was probably a better way. On the PC version, many
players have decided to simply cheese the game by abusing the keyboard controls
or even using a program to automate the entire feat. Doing this on the Switch
is a bit trickier, though.

Luckily I remembered encountering [this blog post][4] in which Bertrand Fan
managed to write a program to win rupees in Zelda for him. Building on the
earlier work of [dekuNukem][5] and [Jason DeLeon][6] to reverse engineer parts
of the Nintendo Switch, an inexpensive [USB development board][7] can be made
to emulate a Switch-compatible controller. With that hard work out of the way,
programming it to emit pre-determined sequences of button presses is relatively
simple.

Making the minimal necessary number of changes to the code, I was able to adapt
Bertrand's Zelda script to complete the dash challenge for me. The [resulting
code][8] is pretty ugly (there's no good reason why dashing 800 times should
require 800 lines of code), but considering it's only meant to be run once I
have no reason to improve it.

Getting the timing right was the trickiest part. Bertrand uses a little DSL to
sequence his button presses that looks like this:

    typedef struct {
        Buttons_t button;
        uint16_t duration;
    } command; 

    static const command step[] = {
        { B,         5 },
        { NOTHING, 150 },
    };

This will be interpreted as instructions to emit the B button for 5 units,
followed by a pause for 150 units. But what are the units of duration? It turns
out that they're simply the number of times the main loop should repeatedly
report the same input to the console. Since the USB polling frequency is 8 ms,
and each USB report is echoed 3 times (for reasons I don't really understand),
then 5 units of duration is approximately equal to 5 \* 8 \* 3 == 120 ms.
Similarly, 150 units is 150 \* 8 \* 3 == 3600 ms, or about 3.6 seconds.


[1]: http://www.zachtronics.com/
[2]: http://www.heart-machine.com/
[3]: https://old.reddit.com/r/hyperlightdrifter/
[4]: https://medium.com/@bertrandom/automating-zelda-3b37127e24c8
[5]: https://github.com/dekuNukem/Nintendo_Switch_Reverse_Engineering
[6]: https://github.com/progmem/Switch-Fightstick
[7]: https://www.pjrc.com/store/teensypp.html
[8]: https://github.com/ajgrf/hyper-light-dasher
