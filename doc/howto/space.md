
Some elements describe several events, and they sometimes need that at least some amount of time pass between the two events.
An instance of such a scheme can be found in [secrets.murder](../../data/elements/secrets.md):
```murder
element MetADreamTravellerInDream
begin
  let P be player
  let D be player with attribute SecretSociety as DreamTravellers

  (* ... *)

  provide event lasting days to P
  begin
    event Dream

    (* The character P meets D in a dream. *)
  end

  provide phantom event lasting weeks to P and D

  provide event lasting minutes to P
  begin
    event Dream

    (* They meet again. *)
  end
end
```
This element presents several events:
- one when the two characters `P` and `D` meet for the first time,
- another one when they meet again.
For this element to work, these to events need to be separated by some time.
This is done by the mean of a `phantom` event whose duration indicates a minimum amount of time passed between the two events.
Nothing is associated with this phantom event (no translation, constraints, or event kind) and there is thus no need to open a block for it: it just consists of `provide phantom event lasting weeks to P and D`.

This works because each events in an element are assured to be ordered consistently with their ordering in the element: the first event will occur before the second one, and so on.
There is however no way to ensure that two events have a maximum of time between the two.
The generator tries to be conservative, though: if there is no reason to add much space between two events, it won’t add it.

For reference, here are the possible values that can come after the `lasting` keyword: `seconds`, `minutes`, `days`, `weeks`, `years`, `decades`.
If you need more than that, you can use multiple phantom events.
This is for instance used in [identity.murder](../../data/elements/identity.md) to ensure that quite a long time is passed between the birth of a character and the current date:
```murder
element BirthOld
begin
  let P be player

  provide immediate event to P
  begin
    provide strict attribute Born to P as True

    translation en P:+sbeg " is born."
  end

  provide blocking phantom event lasting decades to P

  provide blocking phantom event lasting decades to P

  provide blocking phantom event lasting decades to P
  begin
    assume no event Personal to P before

    provide attribute Age to P as Old
  end
end
```
Notice how the `blocking` command prevents other events to be associated to `P` at the same time than these events.
Other events may technically be placed between these blocking events, but the last event comes with the command `assume no event Personal to P before`, ensuring that such event happening before are not of the kind `Personal` (which most event are).
Note also that this trick of adding phantom events can be used to ensure that an event (in this case, the birth of a character) happens enough time in the past.

