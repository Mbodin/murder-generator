
This tutorial aims at creating your first scenario elements.
It is advised to have already generated some scenarios with the generator to roughly understand how it works.
The file [elements.md](../explanations/elements.md) provides a more detailed explanation, but also more technical: it might be a good read if you want to understand what the commands of this tutorial are doing.

# Prerequisite

Before actually defining our own scenario element, why would you want to define a scenario element?
This program is generating scenarios by combining scenario elements: the more diverse and interesting scenario elements are, the better the generated scenarios.

Great!
So let us define a scenario element.

But wait, what does a scenario element looks like?
Well, there are quite different kinds of scenario elements.
Some are extremely small and just state that someone’s gender or job without further explanations.
Some are quite complex, and are small stories that link different parts of a scenario.
Some are just little stories within the scenario.
We will focus in this tutorial on this last kind.

But first of all, you have to download the source.
In this tutorial, we shall assume that you have some experience with a UNIX terminal: we shall only show commands for Ubuntu and assume that you can work with this.
Type these commands to download the source and its dependencies:
```bash
git clone https://github.com/Mbodin/murder-generator.git
cd murder-generator
sudo apt install npm
npm install --global esy
esy install
```

Excellent: now you have all the source and its dependencies.
The scenario elements are all in the folder [elements](../../data/elements) folder, so let us get there.
```bash
cd data/elements
```

Now we are ready to write our first scenario elements.


# First Scenario Element

Let us start with something simple, with only one character.
Let us call this character `A` here.
This character woke up one day, and he/she felt that the world could be made better.
Let us tell this character to conquer the world!

First, we have to choose a file to put this element.
It could be any `.murder` file.
For instance [wars.murder](../../data/elements/wars.murder): conquering the world certainly involves some wars.
But anyway, which file you choose has no importance, as soon as it is a `.murder` file.
```bash
gedit wars.murder
```

Go to the very end of the file, and add the following lines:
```murder
element MyFirstScenarioElement
begin
let A be player
```
These lines tell the program that you are defining a scenario element involving `A`.
The name `MyFirstScenarioElement` could be anything as soon as it starts with an uppercase letter.

Scenario elements are centered around the notion of events.
So let us add an event:
```murder
provide event lasting minutes to A
begin
event Personal
```

Now that we are defining an event, we can define sentences.
Here is an example:
```murder
sentence
begin
translation en "Today is a strange day: birds are flying low, but no cloud taint the sky."
end
```
The `en` flag is the language code for English.
You can of course write sentences in other languages, or even write several translations for the same sentence: just add a translation in another language with its language code.
The language code is the usual two-letter representation of your language (`de` for German, `eo` for Esperanto, `zh` for Chinese, etc.).
See [this page](https://en.wikipedia.org/wiki/List_of_ISO_639-1_codes) for the list of language codes.
If you don’t find a two-letter language code for your language, use the three-letter language code referenced in [this page](https://en.wikipedia.org/wiki/List_of_ISO_639-2_codes).

Cool, we can write sentences.
But wait: we want to include `A` in my sentence, after all it’s his/her event.
Except that `A` is not his/her real name: this name is chosen by the user at generation time, after all.
No problem: just place `A` outside quotes.
Here is an example:
```murder
sentence
begin
translation en "This seemed all too obvious for " A ": it is time to conquer the world!"
end
```
The program will replace `A` by the actual name of the character.
So if `A` is actually named `John` at generation time, the program will generate the sentence `This seemed all too obvious for John: it is time to conquer the world!`.

This works well if the name `John` is to be placed in the middle of the sentence.
But what if the sentence has to start with `A`’s name?
Place it directly before the first quote:
```murder
sentence
begin
translation en A " got up and started to think about it."
end
```

Sure.
But sometimes we don’t want to place the name of `A`, but a pronoun or an other grammatical entity representing `A`.
This is language dependent: as the grammatical construct for each language are different, there is no general rule.
In this case, it would be `A:+pro` (or `A:+pro:+sbeg` if at the beginning of a sentence), but it varies from language to language.
See [language.md](../explanations/language.md) for an explanation on how it works.
In our case, we would write:
```murder
sentence
begin
translation en A:+pro:+sbeg " got up and started to think about it."
end
```
This sentence will then be instantiated as `He got up and started to think about it.` or `She got up and started to think about it.` depending on the context.

We could continue describing why `A` wants to conquer the world so much, but this is only a small tutorial: let us just finish this element.
We have to close the event, but also the element:
```murder
end
end
```

We can add some space to make it more readable:

```murder
element MyFirstScenarioElement
begin
  let A be player

  provide event lasting minutes to A
  begin
    event Personal

    sentence
    begin
      translation en "Today is a strange day: birds are flying low, but no cloud taint the sky."
    end

    sentence
    begin
      translation en "This seemed all too obvious for " A ": it is time to conquer the world!"
    end

    sentence
    begin
      translation en A:+pro:+sbeg " got up and started to think about it."
    end
  end
end
```
This element is valid: if you save the program and use it as-is, it may be used from time to time in the generated scenarios.

Congratulations: you just wrote your first scenario element!
It is of course possible to create more elaborate scenario elements: writing elements with more than one character, with more than one event, or with elaborate constraints on these characters and events.
Browse [the documentation](../index.md) for more information.

