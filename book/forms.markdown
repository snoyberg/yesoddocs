I've mentioned the boundary issue already: whenever data enters or leaves our application, we need to validate our data. Probably the most difficult place this occurs is forms. Coding forms has a lot of complexities in it; in an ideal world, we'd like a solution that addresses the following problems:

* Ensure data is valid.

* Marshal string data in the form submission to Haskell datatypes.

* Generate HTML code for displaying the form.

* Generate Javascript to do clientside validation and provide more user-friendly widgets, such as date pickers.

* Build up more complex forms by combining together simpler forms.

The form system used in Yesod is built around many of the concepts in formlets; if you are familiar with that work, it will give you a head start here. In this chapter, we'll start with some real-life examples of using forms, and then get into the low-level details of how they work so you can abuse their full power.

## Random bananas

Let's start off with a silly example: we want to create a website that generates a random number of some object. It needs to know the minimum number of objects, the maximum number of objects, the name of a single object and the name of a plural object (eg, book and books, mouse and mice).

~forms-random1

In lines 1-6, we set up a new datatype to hold all of the parameter information we need for this program. There's nothing special at all about this datatype, it's a <abbr title="Plain Old Haskell Datatype">POHD</abbr>. Lines 8-13 is where we introduce the forms API in Yesod.

First, notice the type signature: this function takes a <code>Maybe Params</code>. This allows us to either start with a blank form (<code>Nothing</code>) or initialize our form to some default value. This can be especially useful when creating a add/edit interface. The return type of the function is a Form; we'll deal more with that later. Also, since this pattern of taking a Maybe value is so common, it has a synonym defined as <code>Formlet</code> (line 9).

Let's skip down to line 11: we start off with <code>&lt;$&gt;</code>: this stresses the fact that we deal with forms using their <code>Applicative</code> instance. This allows us to check *all* fields in a form for validation errors. Lines 12-14 all start with <code>&lt;*&gt;</code>; you can see more information on the Applicative typeclass on the [wikibook Applicative page](http://en.wikibooks.org/wiki/Haskell/Applicative_Functors).

Continuing along line 11, we call <code>intField</code>. This is doing exactly what you expect: specifying we want an integral value. "Minimum number" is the label displayed to the user, and the last part of the line gives the initial value of the field.

So how exactly is this initial value used? Well, if the user submits a value for the field, the initial value is ignored entirely. If there is *not* a user-submitted value (eg, this is the first time we are showing the user the form), if an initial value is given, then that is put in the field. If no initial value is provided, the field starts off blank.

Lines 12-14 should be easy enough to understand: they are almost identical to line 11. Returning back to line 10, we see a call to <code>fieldsToTable</code>. This function displays our form fields as a table, with one field per row. We have a few other options available, but this will most likely be the one you use most.

Moving on to the handler function, on line 18 we call our <code>paramsFormlet</code> function with a <code>Nothing</code> argument: this means the form starts of blank. We pass this value to <code>runFormGet</code>, which binds our form to GET (aka, query-string) parameters. (Before you ask, yes, there is also <code>runFormPost</code>.) The return type of that function is a 3-tuple, containing:

1. The result of validating the form. We see below (lines 21-23) that there are three constructors for a <code>FormResult</code>: FormMissing for when no data was submitted, FormFailure for validation errors, and FormSuccess when everything went OK.

2. The form itself, as a <code>Widget</code>. This value will include inline validation errors, the previously submitted values, labels, etc. Notably, it does *not* include the &lt;table&gt; or &lt;form&gt; tags; you can look at the Hamlet template (lines 28-34) to see why that is.

3. The value for the enctype attribute of the form. Unless you have file submissions, this will be url-encoded.

### Something not there to be noticed

Did you notice that at no point did I specify the name attribute for any of those fields? There's actually a bit of magic at play here, so let's unravel it. First: Yesod can automatically provide unique names to forms, and by default it does that. Let's have a look at the generated code (newlines and tabbing added for convenience; Hamlet always spits out condensed code):

    <form>
      <table>
        <tr>
          <td><label for="f3">Minimum number</label><div class="tooltip"></div></td>
          <td><input id="f3" name="f2" type="number" required value=""></td>
        </tr>
        <tr>
          <td><label for="f5">Maximum number</label><div class="tooltip"></div></td>
          <td><input id="f5" name="f4" type="number" required value=""></td>
        </tr>
        <tr>
          <td><label for="f7">Single word</label><div class="tooltip"></div></td>
          <td><input id="f7" name="f6" type="text" required value=""></td>
        </tr>
        <tr>
          <td><label for="f9">Plural word</label><div class="tooltip"></div></td>
          <td><input id="f9" name="f8" type="text" required value=""></td>
        </tr>
        <tr>
          <td colspan="2"><input type="submit" value="Randomize!"></td>
        </tr>
      </table>
    </form>

We can see these f2, f3 and so on names and ids have been sprinkled everywhere. This can be incredibly convenient: we never have to worry about coming up with good, unique names. But what if we *want* to manually specify names and ids? And additionally, what's up with those tooltip divs?

The answer to both questions is that our original code involved a little bit of a trick. If you look back at line 11, the first argument to intField is "Minimum number." But if you look at the [API docs for intField](http://hackage.haskell.org/packages/archive/yesod/0.5.2/doc/html/Yesod-Form-Fields.html#v:intField), you'll see that the first argument is something called [FormFieldSettings](http://hackage.haskell.org/packages/archive/yesod/0.5.2/doc/html/Yesod-Form-Core.html#t:FormFieldSettings).

To make life easier for you, Yesod defines an IsString instance for FormFieldSettings, and if you turn on OverloadedStrings, you can pretend that the first argument is just a string. But if you want to, you can supply a FormFieldSettings value directly. Case in point, we could replace line 13 with the following:

     <*> stringField FormFieldSettings
            { ffsLabel = "Single word"
            , ffsTooltip = "The singular version of the object, eg mouse versus mice"
            , ffsId = Just "single-word"
            , ffsName = Just "single-word"
            } (fmap singleWord mparams)

<p class="advanced">You may have noticed that the minimum and maximum number input fields have a type="number" attribute. This is an example of Yesod including HTML 5 support by default. On some browsers (Chrome, for instance) this field gets rendered with up/down errors to control to value, plus it prevents non-numeric input. On browsers without any specific support, it gets rendered as a plain type="text" input. [diveintohtml5 has a chapter on forms](http://diveintohtml5.org/forms.html).</p>

## Custom fields

Yesod comes built in with a large number of fields, for dates, numbers, booleans, lists, etc. It also provides "maybe" variants for most of these, allowing blank values. However, occassionally you really will want to write your own, custom field. Plus, writing a custom field is a great way to see the internals, so let's hop to.

There's two slightly clumsy things about our previous example: it doesn't give a validation error when the minimum number is greater than the maximum, and it would be nice to show both numbers on the same row in the table. Let's go ahead and fix both issues.

Just a forewarning: this code looks complicated, but it's really more tedious than anything else. The point of the Yesod form library is to alleviate the monotonous tasks involved in web forms. Unfortunately, that tedious work still has to happen *somewhere*.

~forms-random2

One line 2 we can see the relevant change to the Params datatype. Technically speaking, we could have left the datatype the same as before, but that would have made the body of the paramsFormlet function more complicated; I leave that as an exercise to the reader.

On line 9, we now call the rangeField function (which we are about to define). This function does not take a FormFieldSettings argument; instead, the label, ids and names are all explicitly defined in the rangeField function itself.

Lines 13-15 give three equivalent type signatures for rangeField. Let's point out a few important pieces:

* FieldInfo is a datatype which allows us to restructure a form in different ways. For example, fieldsToTable is able to convert this datatype into an HTML table. FieldInfo contains such information as the label, tooltip, HTML of the input area and validation errors. We create a value of it on lines 37-51.

* Similar to GHandler and GWidget, GForm is a generic form. This should give a hint to the meaning of the s and m parameters: these are the sub site and master site, respectively. Usually, this is not important. Sometimes, however, we may want to do more fancy things such as loading a list of possible values from a database, in which case we will need to pay attention to those parameters.

* And as before, Maybe + Form = Formlet. Additionally, we have the word Field tacked on in lines 14 and 15, to indicate that we have a FieldInfo.

Lines 17-20 acquire our unique ids and names. On line 21, askParams acquires the parameters the user submitted. Since this form gets run eventually with runFormGet, this receives the query-string (GET) parameters; if we used runFormPost, it would receive the request body (POST) parameters. Lines 22-34 introduce the tedious part of form checking; we need to do the following:

1. If there are no form parameters at all, then assume the user didn't submit the form and return a FormMissing (line 24).

2. If either of the minName and maxName parameters are missing, return a FormFailure indicating the the range is required (line 34).

3. If either of the parameters do not parse an integers, return a FormFailure (line 33).

4. Now that we know we have two integers, check if minimum is greater than maximum. If so, return a FormFailure (line 31). Otherwise, we have a success (line 32).

<p class="advanced">Note that technically we should check in step 2 if the user submitted an empty string and consider that the same as not submitting a parameter at all. To make things a little simpler in the example, we ignored this case, since the empty string will be caught by step 3 anyway.</p>

Lines 35 and 36 set up the String "value" attribute for the minimum and maximum fields. The code may look a little tricky, but we essentially do the following:

1. If the use submitted a value, use it.

2. Otherwise, use the initial value.

3. If there is no initial value, use a blank.

Next we create a FieldInfo value. The one really confusing piece is the value of fiIdent: why did I choose minId and not maxId? The value of fiIdent gets used when constructing the final HTML, as the value of the "for" attribute on the label tag. By setting the value to minId, it means that when a user clicks on the label, the browser will shift focus to the minimum field, which is probably what we want.

The values for fiInput and fiErrors are, once again, tedious but straight-forward. We can now put both of our input fields together, and if we created any FormFailure above, we display it in the fiErrors. Line 52 returns our 3-tuple of form result, FieldInfo and encoding type.

## Automatic Javascript goodness

After seeing that last example, you may be questioning whether forms are really worth it. Overall, I think that creating a new field is about as difficult as writing all of the code "from scratch", without the forms library. But once you have the fields, using them is *much* simpler than the raw approach. And most of the time, the built in fields are sufficient.

Just to leave you with a good taste in your mouth, let's see an example where the combination of forms and widgets can give you beautiful forms very easily.

~forms-js

On lines 1 and 2, we declare two new instances: YesodJquery and YesodNic. These typeclasses allow us to specify where to get the relevant Javascript libraries and the attached CSS files. By default, they download them from CDNs. Line 3 starts our <code>Survey</code> datatype. On lines 9 through 14, we set up our surveyFormlet, very similar to our formlet from the previous two examples. The jqueryAutocompleteField function takes a *route* as its first argument, of where to get the autocomplete results. The rest of the example is fairly boilerplate.

What's awesome about this example is that no where did we pull in the Javascript libraries: it was taken care of for us automatically. This is the huge advantage of widgets: we are able to package up some complete functionality in one module and use it with a single line of code in another. All of the field set up code was also handled for us automatically.

## Summary

Forms are a complicated part of web programming, involving a lot of monotony. The form library in Yesod can turn this error-prone process into a nice, declarative experience. Teamed up with widgets, you can even create advanced Javascript-powered UIs without a sweat.
