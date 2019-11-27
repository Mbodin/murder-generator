
In order to avoid too much chaos or to prepare for a specific background, some scenario elements state that all characters are following a particular scheme.
For instance, let us consider the following element `NoGod` from [religion.murder](../../data/elements/religion.murder).
```murder
unique element NoGod
begin
  let any other player be with attribute Specy not as God
end
```
This is an unusual element: it assumes no characters, provides no event, attribute, contact, or relation.
It however only applies if some constraints on the other characters (that is, all the scenario’s characters, as no characters are assumed in this element) are not gods.
This means that if this element is applied by the generator, it forces all players not to be gods.
The following element `AllHumans` from [identity.murder](../../data/elements/identity.murder) is very similar, ensuring that all characters are indeed human:
```murder
unique element AllHumans
begin
  let any other player be with attribute Specy as Human
end
```
Note that these two elements may not apply.
But if they are indeed applied, there will be no god (respectively, only human beings) in the scenario.

Such elements can be useful to provide some particular atmosphere (by enforcing a particular theme to all players).
Note that sometimes, one just want to ensure that most characters respect some constraints, but are fine if they are some exceptions.
In such cases, just declare characters: the `any other player` will be restricted accordingly.
For instance the following element `MostlyHumans` from [identity.murder](../../data/elements/identity.murder) ensures that all players but four are human beings:
```murder
unique element MostlyHumans
begin
  let A be player
  let B be player
  let C be player
  let D be player
  let any other player be with attribute Specy as Human
end
```
This element states nothing about these four characters: they can be something else than a human being, but they also can be human.

The `unique` keyword helps the generator to know that this element is not meant to be applied more than once (it wouldn’t provide any other information anyway).
It is not necessary, but it makes the generation (slightly) faster.

The elements above assume some properties about the state, constraining it.
It is also possible to actively enforce these constraints by providing attributes or contacts, as can be seen in the following element `NotAFamilyRelationExcept` in [family.murder](../../data/elements/family.murder):
```murder
unique element NotAFamilyRelationExcept
begin
  let A be player
  let B be player
  let C be player

  provide compatible contact FamilyRelation between A and any other player as None
  provide compatible contact FamilyRelation between B and any other player as None
  provide compatible contact FamilyRelation between C and any other player as None
end
```
This `provide compatible contact FamilyRelation` references `any other player`, enforcing the contact to all characters but `A`, `B`, and `C`.
(If one doesn’t want to exclude `A`, `B`, and `C`, there is also an expression `any player` that includes them.)
Such a contact is here not associated with an event or any explanation: it is important to mark it as `compatible` to let other elements be applied.
The previous elements above did not need any `compatible` indications as they did not provide anything: they only assume constraints in the state.

In addition to provide some particular atmosphere, such `any other player` annotations can also be useful to avoid embarrassing interactions between elements.
For instance, consider the following element `Twins` from [family.murder](../../data/elements/family.murder):
```murder
element Twins
begin
  let A be player with attribute Specy as Human
  let B be player with attribute Specy as Human
  let any other player be with contact FamilyRelation to A as None
                          with contact FamilyRelation to B as None
```
This element ensures that there is no family relation between `A`, `B`, and another character.
This for instance avoids having another element stating that another character `C` is both a parent of `A` and a child of `B`… which would be absurd!
(Or which would at least require some additional explanations in a specialised element.)

Also note the possibility of declaring an element as `duplicable`.
For instance `GodYieldStrongFeeling` from [politics.murder](../../data/elements/politics.murder):
```murder
duplicable element KingIsKnownToBeKing
begin
  let K be player with attribute Job as King
  let P be player

  provide immediate event to P
  begin
    assume event providing attribute Job to K before

    translation en K:+sbeg " is the king."
  end
end
```
This elements aim to tell every character that the character `K` is indeed the king.
However, `duplicable` does not ensure that this element will be applied to every other player: it only enables the generator to do so.
It is thus important to keep in mind that such an element may not be applied to all other players.

