
Sometimes, one needs to ensure in an element that a character is older than another.
There is an attribute `Age` defined in [identity.murder](../../data/elements/identity.murder) with values `Young`, `Adult`, and `Old`, but as any other attribute, it doesn’t come with very strong connections with its actual age.
It is thus theoretically possible to have a character older than another marked as `Young` whilst the other character is marked as `Old`.
Such situations can happen when the solver is under pressure, with large amount of constraints.

Instead, the notion of age is intuitively linked to the date of birth, that is to an event, not an attribute.
Thus, we need to ensure that the event providing the attribute `Born` to the first character is placed before the event providing the attribute `Born` of the other character.

If the element that you are currently writting is defining one of these events, just add the corresponding constraint in this event.
It will look like this, ensuring that `B` was born before `A`:
```murder
  provide event lasting minutes to A
  begin
    assume event providing attribute Born to B before

    translation en A " was just born."
  end
```

However, most of the time, such event is not part of the element that you are defining.
Then, create a phantom event: its only purpose will be to express that an event has happenned before another.
Its content just states that the birth of the older character happenned before, and that the birth of the other character happenned after.
See for instance this event of the element `AdultAndOldOlderThanYoung` of [identity.murder](../../data/elements/identity.murder), ensuring that `B` was born before `A`:
```murder
  provide immediate phantom event to A and B
  begin
    assume no event providing attribute Born to A before
    assume no event providing attribute Born to B after
  end
```
This is the most frequent way to ensure such constraint.

