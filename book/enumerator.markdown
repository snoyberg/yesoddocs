Note: I've crudely copied content in here from [three](http://docs.yesodweb.com/blog/enumerators-tutorial-part-1/) [blog](http://docs.yesodweb.com/blog/enumerators-tutorial-part-2/) [posts](http://docs.yesodweb.com/blog/enumerators-tutorial-part-3/) I wrote. The content may require some refining still for book format.

# Part 1: Iteratees

## Introduction

One of the upcoming patterns in Haskell is the enumerators. Unfortunately, it's very difficult to get started with them since:

* There are multiple implementations, all with slightly different approaches.

* Some of the implementations (in my opinion) use incredibly confusing naming.

* The tutorials that get written usually don't directly target an existing implementation, and work more on building up intuition than giving instructions on how to use the library.

I'm hoping that this tutorial will fill the gap a bit. I'm going to be basing this on the [enumerator package](http://hackage.haskell.org/package/enumerator). I'm using version 0.4.0.2, but this should be applicable to older and hopefully newer versions as well. This package is newer and less used than the [iteratee](http://hackage.haskell.org/package/iteratee) package, but I've chosen it for three reasons:

* It has a much smaller dependency list.

* It's a smaller package, and therefore easier to wrap your mind around.

* I think the naming is better.

That said, both packages are built around the same basic principles, so learning one will definitely help you with the other.

### Three Parts

The title of this post says this is part 1. In theory, there will be three parts (though I may do more or less, I'm not certain yet). There are really three main concepts to learn to use the enumerator package: iteratees, enumerators and enumeratees. A basic definition would be:

* Iteratees are *consumers*: they are fed data and do something with it.

* Enumerators are *producers*: they feed data to an iteratee.

* Enumeratees are *pipes*: they are fed data from an enumerator and then feed it to an iteratee.

### What good are enumerators?

But before you really get into this library, let's give some motivation for *why* we would want to use it. Here's some real life examples I use the enumerator package for:

* When reading values from a database, I don't necessarily want to pull all records into memory at once. Instead, I would like to have them fed to a function which will consume them bit by bit.

* When processing a YAML file, instead of reading in the whole structure, maybe you only need to grab the value of one or two records.

* If you want to download a file via HTTP and save the results in a file, it would be a waste of RAM to store the whole file in memory and then write it out. Enumerators let you perform interleaved IO actions easily.

A lot of these problems can also be solved using lazy I/O. However, lazy I/O is not necessarily a panacea: you might want to read some of [Oleg's stuff](http://okmij.org/ftp/Streams.html#iteratee) on the pitfalls of lazy I/O.

## Intuition

*Note: you can see the code in this post as a [github gist](http://gist.github.com/605826).*

Let's say we want to write a function that sums the numbers in a list. Forgetting uninteresting details like space leaks, a perfectly good implementation could be:

    sum1 :: [Int] -> Int
    sum1 [] = 0
    sum1 (x:xs) = x + sum1 xs

But let's say that we don't have a list of numbers. Instead, the user is typing numbers on the command line, and hitting "q" when done. In other words, we have a function like:

    getNumber :: IO (Maybe Int)
    getNumber = do
        x <- readLine
        if x == "q"
            then return Nothing
            else return $ Just $ read x

We could write our new sum function as:

    sum2 :: IO Int
    sum2 = do
        maybeNum <- getNumber
        case maybeNum of
            Nothing -> return 0
            Just num -> do
                rest <- sum2
                return $ num + rest

It's fairly annoying to have to write two completely separate sum functions just because our data source changed. Ideally, we would like to generalize things a bit. Let's start by noticing a similarity between these two functions: they both only **yield** a value when they are informed that there are no more numbers. In the case of sum1, we check for an empty list; in sum2, we check for Nothing.

## The Stream datatype.

The first datatype defined in the enumerator package is:

    data Stream a = Chunks [a] | EOF

The EOF constructor indicates that no more data is available. The Chunks constructor simply allows us to put multiple pieces of data together for efficiency. We could now rewrite sum2 to use this Stream datatype:

    getNumber2 :: IO (Stream Int)
    getNumber2 = do
        maybeNum <- getNumber -- using the original getNumber function
        case maybeNum of
            Nothing -> return EOF
            Just num -> return $ Chunks [num]

    sum3 :: IO Int
    sum3 = do
        stream <- getNumber2
        case stream of
            EOF -> return 0
            Chunks nums -> do
                let nums' = sum nums
                rest <- sum3
                return $ nums' + rest

Not that it's much better than sum2, but at least it shows how to use the Stream datatype. The problem here is that we still refer explicitly to the getNumber2 function, hard-coding the data source.

One possible solution is to make the data source an argument to the sum function, ie:

    sum4 :: IO (Stream Int) -> IO Int
    sum4 getNum = do
        stream <- getNum
        case stream of
            EOF -> return 0
            Chunks nums -> do
                let nums' = sum nums
                rest <- sum4 getNum
                return $ nums' + rest

That's all well and good, but let's pretend we want to have *two* datasources to sum over: values the user enters on the command line, and some numbers we read over an HTTP connection, perhaps. The problem here is one of **control**: sum4 is running the show here by calling getNum. This is a **pull** data model. Enumerators have an **inversion of control/push** model, putting the enumerator in charge. This allows cool things like feeding in multiple data sources, and also makes it easier to write enumerators that properly deal with resource allocation.

## The Step datatype

So we need a new datatype that will represent the state of our summing operation. We're going to allow our operations to be in one of three states:

* Waiting for more data.

* Already calculated a result.

* For convenience, we also have an error state. This isn't strictly necessary (it could be modeled by choosing an EitherT kind of monad, for example), but it's simpler.

As you could guess, these states will correspond to three constructors for the Step datatype. The error state is modeled by <code>Error SomeException</code>, building on top of Haskell's extensible exception system. The already calculated constructor is:

    Yield b (Stream a)

Here, a is the *input* to our iteratee and b is the *output*. This constructor allows us to simultaneously produce a result and save any "leftover" input for another iteratee that may run after us. (This won't be the case with the sum function, which always consumes all its input, but we'll see some other examples that do no such thing.)

Now the question is how to represent the state of an iteratee that's waiting for more data. You might at first want to declare some datatype to represent the internal state and pass that around somehow. That's not how it works: instead, we simply use a function (very Haskell of us, right?):

    Continue (Stream a -> Iteratee a m b)

Euerka! We've finally seen the Iteratee datatype! Actually, Iteratee is a very boring datatype that is only present to allow us to declare cool instances (eg, Monad) for our functions. Iteratee is defined as:

    newtype Iteratee a m b = Iteratee (m (Step a m b))

This is important: **Iteratee is just a newtype wrapper around a Step inside a monad**. Just keep that in mind as you look at definitions in the enumerator package. So knowing this, we can think of the Continue constructor as:

    Continue (Stream a -> m (Step a m b))

That's much easier to approach: that function takes some input data and returns a new state of the iteratee. Let's see what our sum function would look like using this Step datatype:

    sum5 :: Monad m => Step Int m Int -- Int input, any monad, Int output
    sum5 =
        Continue $ go 0 -- a common pattern, you always start with a Continue
      where
        go :: Monad m => Int -> Stream Int -> Iteratee Int m Int
        -- Add the new input to the running sum and create a new Continue
        go runningSum (Chunks nums) = do
            let runningSum' = runningSum + sum nums
            -- This next line is *ugly*, good thing there are some helper
            -- functions to clean it up. More on that below.
            Iteratee $ return $ Continue $ go runningSum'
        -- Produce the final result
        go runningSum EOF = Iteratee $ return $ Yield runningSum EOF

The first real line (<code>Continue $ go 0</code>) initializes our iteratee to its starting state. Just like every other sum function, we need to explicitly state that we are starting from 0 somewhere. The real workhorse is the go function. Notice how we are really passing the state of the iteratee around as the first argument to go: this is also a very common pattern in iteratees.

We need to handle two different cases: when handed an EOF, the go function **must** Yield a value. (Well, it could also produce an Error value, but it definitely **cannot** Continue.) In that case, we simply yield the running sum and say there was no data left over. When we receive some input data via Chunks, we simply add it to the running sum and create a new Continue based on the same go function.

Now let's work on making that function a little bit prettier by using some built-in helper functions. The pattern <code>Iteratee . return</code> is common enough to warrant a helper function, namely:

    returnI :: Monad m => Step a m b -> Iteratee a m b
    returnI = Iteratee . return

So for example,

    go runningSum EOF = Iteratee $ return $ Yield runningSum EOF

becomes

    go runningSum EOF = returnI $ Yield runningSum EOF

But even *that* is common enough to warrant a helper function:

    yield :: Monad m => b -> Stream a -> Iteratee a m b
    yield x chunk = returnI $ Yield x chunk

so our line becomes

    go runningSum EOF = yield runningSum EOF

Similarly,

    Iteratee $ return $ Continue $ go runningSum'

becomes

    continue $ go runningSum'

## Monad instance for Iteratee

This is all very nice: we now have an iteratee that can be fed numbers from any monad and sum them. It can even take input from different sources and sum them together. (By the way, I haven't actually shown you how to feed those numbers in: that is in part 2 about enumerators.) But let's be honest: sum5 is an ugly function. Isn't there something easier?

In fact, there is. Remember how I said Iteratee really just existed to facilitate typeclass instances? This includes a monad instance. Feel free to look at the code to see how that instance is defined, but here we'll just look at how to use it:

    sum6 :: Monad m => Iteratee Int m Int
    sum6 = do
        maybeNum <- head -- not head from Prelude!
        case maybeNum of
            Nothing -> return 0
            Just i -> do
                rest <- sum6
                return $ i + rest

That head function is not from Prelude, it's from the Data.Enumerator module. Its type signature is:

    head :: Monad m => Iteratee a m (Maybe a)

which basically means give me the next piece of input if it's there. We'll look at this function in more depth in a bit.

Go compare the code for sum6 with sum2: they are amazingly similar. You can often build up more complicated iteratees by using some simple iteratees and the Monad instance of Iteratee.

## Interleaved I/O

Alright, let's look at a totally different problem. We want to be fed some strings and print them to the screen one line at a time. One approach would be to use lazy I/O:

    lazyIO :: IO ()
    lazyIO = do
        s <- lines `fmap` getContents
        mapM_ putStrLn s

But this has two drawbacks: 

* It's tied down to a single input source, stdin. This could be worked around with an argument giving a datasource.

* But let's say the data source is some scarce resource (think: file handles on a very busy web server). We have no guarantees with lazy I/O of when those file handles will be released.

Let's look at how to write this in our new high-level monadic iteratee approach:

    interleaved :: MonadIO m => Iteratee String m ()
    interleaved = do
        maybeLine <- head
        case maybeLine of
            Nothing -> return ()
            Just line -> do
                liftIO $ putStrLn line
                interleaved

The liftIO function comes from the transformers package, and simply promotes an action in the IO monad to any arbitrary MonadIO action. Notice how we don't really track any state with this iteratee: we don't care about its result, only its side effects.

## Implementing head

As a last example, let's actually implement the head function.

    head' :: Monad m => Iteratee a m (Maybe a)
    head' =
        continue go
      where
        go (Chunks []) = continue go
        go (Chunks (x:xs)) = yield (Just x) (Chunks xs)
        go EOF = yield Nothing EOF

Like our sum6 function, this also wraps an inner "go" function with a continue. However, we now have *three* clauses for our go function. The first handles the case of <code>Chunks []</code>. To quote the enumerator docs:

> (Chunks []) is used to indicate that a stream is still active, but currently has no available data. Iteratees should ignore empty chunks.

The second clause handles the case where we are given some data. In this case, we yield the first element in the list, and return the rest as leftover data. The third clause handles the end of input by returning Nothing.

## Exercises

* Rewrite sum6 using [liftFoldL'](http://hackage.haskell.org/packages/archive/enumerator/0.4/doc/html/Data-Enumerator.html#v:liftFoldL-39-).

* Implement the [consume](http://hackage.haskell.org/packages/archive/enumerator/0.4/doc/html/Data-Enumerator.html#v:consume) function using first the high-level functions like head, and then using only low-level stuff.

* Write a modified version of consume that only keeps every other value, once again using high-level functions and then low-level constructors.

## Next time

Well, now you can write iteratees, but they're not very useful if you can't actually use them. Next time we'll cover what an enumerator is, some basic enumerators included with the package, how to run these things and how to write your own enumerator.

## Summary

Here's what I consider the most important things to glean from this tutorial:

* Iteratee is a simple wrapper around the Step datatype to allow for cool typeclass instances.

* Using the Monad instance of Iteratee can allow you to build up complicated iteratees from simpler ones.

* The three states an enumerator can be in are Continue (still processing data), Yield (a result is ready) and Error (duh).

* Well behaved iteratees will never return a Continue after receiving an EOF.

# Part 2: Enumerators

Note: code for this tutorial is available as [a github gist](http://gist.github.com/607856).

## Extracting a value

When we finished the [last part of our tutorial](http://docs.yesodweb.com/blog/enumerators-tutorial-part-1/), we had written a few iteratees, but we still didn't know how to extract values from them. To start, let's remember that Iteratee is just a newtype wrapper around Step:

    newtype Iteratee a m b = Iteratee { runIteratee :: m (Step a m b) }

First we need to unwrap the Iteratee and deal with the Step value inside. Remember also that Step has three constructors: Continue, Yield and Error. We'll handle the Error constructor by returning our result in an Either. Yield already provides the data we're looking for.

The tricky case is Continue: here, we have an iteratee that is still expecting more data. This is where the EOF constructor comes in handy: it's our little way to tell the iteratee to finish what it's doing and get on with things. If you remember from the last part, I said a well-behaving iteratee will never return a Continue after receiving an EOF; now we'll see why:

    extract :: Monad m => Iteratee a m b -> m (Either SomeException b)
    extract (Iteratee mstep) = do
        step <- mstep
        case step of
            Continue k -> do
                let Iteratee mstep' = k EOF
                step' <- mstep'
                case step' of
                    Continue _ -> error "Misbehaving iteratee"
                    Yield b _ -> return $ Right b
                    Error e -> return $ Left e
            Yield b _ -> return $ Right b
            Error e -> return $ Left e

Fortunately, you don't need to redefine this yourself: enumerator includes both a [run](http://hackage.haskell.org/packages/archive/enumerator/0.4.0.2/doc/html/Data-Enumerator.html#v:run) and [run\_](http://hackage.haskell.org/packages/archive/enumerator/0.4.0.2/doc/html/Data-Enumerator.html#v:run_) function. Let's go ahead and use it on our sum6 function:

    main = run_ sum6 >>= print

If you run this, the result will be 0. This emphasizes an important point: an iteratee is not just *how* to process incoming data, **it is the state of the processing**. In this case, we haven't done anything to change the initial state of sum6, so we still have the initial value of 0.

To give an analogy: think of an iteratee as a machine. When you feed it data, you modify the internal state but you can't see any of those changes on the outside. When you are done feeding the data, you press a button and it spits out the result. If you don't feed in any data, your result is the initial state.

## Adding data

Let's say that we actually want to sum some numbers. For example, the numbers 1 to 10. We need some way to feed that into our sum6 iteratee. In order to approach this, we'll once again need to unwrap our Iteratee and deal with the Step value directly.

In our case, we know with certainty that the Step constructor we used is Continue, so it's safe to write our function as:

    sum7 :: Monad m => Iteratee Int m Int
    sum7 = Iteratee $ do
        Continue k <- runIteratee sum6
        runIteratee $ k $ Chunks [1..10]

But in general, we won't know what constructor will be lying in wait for us. We need to properly deal with Continue, Yield and Error. We've seen what to do with Continue: feed it the data. With Yield and Error, the right action in general is to **do nothing**, since we've already arrived at our final result (either a successful Yield or an Error). So the "proper" way to write the above function is:

    sum8 :: Monad m => Iteratee Int m Int
    sum8 = Iteratee $ do
        step <- runIteratee sum6
        case step of
            Continue k -> runIteratee $ k $ Chunks [1..10]
            _ -> return step

## Enumerator type synonym

What we've done with sum7 and sum8 is perform a transformation on the Iteratee. But we've done this in a very limited way: we've hard-coded in the original Iteratee function (sum6). We could just make this an argument to the function:

    sum9 :: Monad m => Iteratee Int m Int -> Iteratee Int m Int
    sum9 orig = Iteratee $ do
        step <- runIteratee orig
        case step of
            Continue k -> runIteratee $ k $ Chunks [1..10]
            _ -> return step

But since we always just want to unwrap the Iteratee value anyway, it turns out that it's more natural to make the argument of type Step, ie:

    sum10 :: Monad m => Step Int m Int -> Iteratee Int m Int
    sum10 (Continue k) = k $ Chunks [1..10]
    sum10 step = returnI step

This type signature (take a Step, return an Iteratee) turns out to be very common:

    type Enumerator a m b = Step a m b -> Iteratee a m b

Meaning sum10's type signature could also be expressed as:

    sum10 :: Monad m => Enumerator Int m Int

Of course, we need some helper function to connect an Enumerator and an Iteratee:

    applyEnum :: Monad m => Enumerator a m b -> Iteratee a m b -> Iteratee a m b
    applyEnum enum iter = Iteratee $ do
        step <- runIteratee iter
        runIteratee $ enum step

Let me repeat the intuition here: the Enumerator is transforming the Iteratee from its initial state to a new state by feeding it more data. In order to use this function, we could write:

    run_ (applyEnum sum10 sum6) >>= print

This results in 55, exactly as we'd expect. But now we can see one of the benefits of enumerators: we can use multiple data sources. Let's say we have another enumerator:

    sum11 :: Monad m => Enumerator Int m Int
    sum11 (Continue k) = k $ Chunks [11..20]
    sum11 step = returnI step

Then we could simply apply both enumerators:

    run_ (applyEnum sum11 $ applyEnum sum10 sum6) >>= print

And we would get the result 210. (Yes, (1 + 20) * 10 = 210.) But don't worry, you don't need to write this applyEnum function yourself: enumerator provides a [$$](http://hackage.haskell.org/packages/archive/enumerator/0.4.0.2/doc/html/Data-Enumerator.html#v:-36--36-) operator which does the same thing. Its type signature is a bit scarier, since it's a generalization of applyEnum, but it works the same, and even makes code more readable:

    run_ (sum11 $$ sum10 $$ sum6) >>= print

<code>&#36;&#36;</code> is a synonym for <a href="http://hackage.haskell.org/packages/archive/enumerator/0.4.0.2/doc/html/Data-Enumerator.html#v:-61--61--60--60-">==&lt;&lt;</a>, which is simply <code>flip <a href="http://hackage.haskell.org/packages/archive/enumerator/0.4.0.2/doc/html/Data-Enumerator.html#v:-62--62--61--61-">&gt;&gt;==</a></code>. I find <code>&#36;&#36;</code> the most readable, but <abbr title="your mileage my vary">YMMV</abbr>.

## Some built-in enumerators

Of course, writing a whole function just to pass some numbers to our sum function seems a bit tedious. We could easily make the list an argument to the function:

    sum12 :: Monad m => [Int] -> Enumerator Int m Int
    sum12 nums (Continue k) = k $ Chunks nums
    sum12 _ step = returnI step

But now there's not even anything Int-specific in our function. We could easily generalize this to:

    genericSum12 :: Monad m => [a] -> Enumerator a m b
    genericSum12 nums (Continue k) = k $ Chunks nums
    genericSum12 _ step = returnI step

And in fact, enumerator comes built in with the [enumList](http://hackage.haskell.org/packages/archive/enumerator/0.4.0.2/doc/html/Data-Enumerator.html#v:enumList) function which does this. enumList also takes an Integer argument to indicate the maximum number of elements to stick in a chunk. For example, we could write:

    run_ (enumList 5 [1..30] $$ sum6) >>= print

(That produces 465 if you're counting.) The first argument to enumList should never affect the result, though it may have some performance impact.

Data.Enumerator includes two other enumerators: [enumEOF](http://hackage.haskell.org/packages/archive/enumerator/0.4.0.2/doc/html/Data-Enumerator.html#v:enumEOF) simply passes an EOF to the iteratee. [concatEnums](http://hackage.haskell.org/packages/archive/enumerator/0.4.0.2/doc/html/Data-Enumerator.html#v:concatEnums) is slightly more interesting; it combines multiple enumerators together. For example:

    run_ (concatEnums
            [ enumList 1 [1..10]
            , enumList 1 [11..20]
            , enumList 1 [21..30]
            ] $$ sum6) >>= print

This also produces 465.

## Some non-pure input

Enumerators are much more interesting when they aren't simply dealing with pure values. In the first part of this tutorial, we gave the example of the user entering numbers on the command line:

    getNumber :: IO (Maybe Int)
    getNumber = do
        x <- getLine
        if x == "q"
            then return Nothing
            else return $ Just $ read x

    sum2 :: IO Int
    sum2 = do
        maybeNum <- getNumber
        case maybeNum of
            Nothing -> return 0
            Just num -> do
                rest <- sum2
                return $ num + rest

We referred to this as the pull-model: sum2 pulled each value from getNumber. Let's see if we can rewrite getNumber to be a pusher instead of a pullee.

    getNumberEnum :: MonadIO m => Enumerator Int m b
    getNumberEnum (Continue k) = do
        x <- liftIO getLine
        if x == "q"
            then continue k
            else k (Chunks [read x]) >>== getNumberEnum
    getNumberEnum step = returnI step

First, notice that we check which constructor was passed, and only perform any actions if it was Continue. If it was Continue, we get the line of input from the user. If the line is "q" (our indication to stop feeding in values), we do nothing. You *might* have thought that we should pass an EOF. But if we did that, we'd be preventing other data from being sent to this iteratee. Instead, we simply return the original Step value.

If the line was not "q", we convert it to an Int via read, create a Stream value with the Chunks datatype, and pass it to k. (If we wanted to do things properly, we'd check if x is really an Int and use the Error constructor; I leave that as an exercise to the reader.) At this point, let's look at type signatures:

    k (Chunks [read x]) :: Iteratee Int m b

If we simply left off the rest of the line, our program would typecheck. However, it would only ever read one value from the command line; the <code>&gt;&gt;== getNumberEnum</code> causes our enumerator to loop.

One last thing to note about our function: notice the b in our type signature.

    getNumberEnum :: MonadIO m => Enumerator Int m b

This is saying that our Enumerator can feed <code>Int</code>s to any Iteratee accepting <code>Int</code>s, and it doesn't matter what the final output type will be. This is in general the way enumerators work. This allows us to create drastically different iteratees that work with the same enumerators:

    intsToStrings :: (Show a, Monad m) => Iteratee a m String
    intsToStrings = (unlines . map show) `fmap` consume

And then both of these lines work:

    run_ (getNumberEnum $$ sum6) >>= print
    run_ (getNumberEnum $$ intsToStrings) >>= print

## Exercises

* Write an enumerator that reads lines from stdin (as Strings). Make sure it works with this iteratee:

    <pre>printStrings :: Iteratee String IO ()
    printStrings = do
        mstring <- head
        case mstring of
            Nothing -> return ()
            Just string -> do
                liftIO $ putStrLn string
                printStrings</pre>

* Write an enumerator that does the same as above with words (ie, delimit on any whitespace). It should work with the same Iteratee as above.

* Do proper error handling in the getNumberEnum function above when the string is not a proper integer.

* Modify getNumberEnum to pull its input from a file instead of stdin.

* Use your modified getNumberEnum to sum up the values in two different files.

## Summary

* An enumerator is a **step transformer**: it feeds data into an iteratee to produce a new iteratee with an updated state. 

* Multiple enumerators can be fed into a single iteratee, and we finally use the run and run_ functions to extract results.

* We can use the $$, >>== and ==<< operators to apply an enumerator to an iteratee.

* When writing an enumerator, we only feed data to an iteratee in the Continue state; Yield and Error already represent final values.

# Part 3: Enumeratees

This is part 3 of a series of tutorials on the [enumerator package](http://hackage.haskell.org/package/enumerator). As usual, the code for this part of the tutorial is available as [a github gist](http://gist.github.com/615786).

## Generalizing getNumberEnum

In [part 2](http://docs.yesodweb.com/blog/enumerators-tutorial-part-2/) of this series, we created a getNumberEnum function with a type signature:

    getNumberEnum :: MonadIO m => Enumerator Int m b

If you don't remember, this means getNumberEnum produces a stream of <code>Int</code>s. In particular, our getNumberEnum function read lines from stdin, converted them to ints and fed them into an iteratee. It stopped reading lines when it saw a "q".

But this functionality seems like it could be useful outside the realm of Ints. We may like to deal with the original Strings, for example, or Bools, or a bunch of other things. We could easily define a more generalized function which simply doesn't do the String to Int conversion:

    lineEnum :: MonadIO m => Enumerator String m b
    lineEnum (Continue k) = do
        x <- liftIO getLine
        if x == "q"
            then continue k
            else k (Chunks [x]) >>== lineEnum
    lineEnum step = returnI step

Cool, let's plug this into our sumIter function (I've renamed the sum6 function from the previous two parts):

    lineEnum $$ sumIter

Actually, that doesn't type check: lineEnum produces <code>String</code>s, and sumIter takes <code>Int</code>s. We need to modify one of them somehow.

    sumIterString :: Monad m => Iteratee String m Int
    sumIterString = Iteratee $ do
        innerStep <- runIteratee sumIter
        return $ go innerStep
      where
        go :: Monad m => Step Int m Int -> Step String m Int
        go (Yield res _) = Yield res EOF
        go (Error err) = Error err
        go (Continue k) = Continue $ \strings -> Iteratee $ do
            let ints = fmap read strings :: Stream Int
            step <- runIteratee $ k ints
            return $ go step

What we've done here is wrap around the original iteratee. As usual, we first need to unwrap the Iteratee constructor and the monad to get at the heart of the Step value. Once we have that innerStep value, we pass it to the go function, which simply transforms that values in the Stream value from Strings to Ints.

## Even more general

Of course, it would be nice if we could apply this transformation to *any* iteratee. To start with, let's just pass the inner iteratee and the mapping function as parameters.

    mapIter :: Monad m => (aOut -> aIn) -> Iteratee aIn m b -> Iteratee aOut m b
    mapIter f innerIter = Iteratee $ do
        innerStep <- runIteratee innerIter
        return $ go innerStep
      where
        go (Yield res _) = Yield res EOF
        go (Error err) = Error err
        go (Continue k) = Continue $ \strings -> Iteratee $ do
            let ints = fmap f strings
            step <- runIteratee $ k ints
            return $ go step

We could call this like:

    run_ (lineEnum $$ mapIter read sumIter) >>= print

Nothing much to see here, it's basically identical to the previous version. What's funny is that enumerator comes built in with a <code>map</code> function to do just this, but it has a significantly different type signature:

    map :: Monad m => (ao -> ai) -> Enumeratee ao ai m b

since:

    type Enumeratee aOut aIn m b = Step aIn m b -> Iteratee aOut m (Step aIn m b)

that's equivalent to:

    map :: Monad m => (aOut -> aIn) -> Step aIn m b -> Iteratee aOut m (Step aIn m b)

What's with all this extra complication in type signature? Well, it's not necessary for map itself, but it *is* necessary for a whole bunch of other similar functions. But let's focus on this map for a second so we don't get lost: the first argument is the same old mapping function we had before. The second argument is a Step value. This isn't really so surprising: in our mapIter, we took an Iteratee with the same parameters, and we internally just unwrapped it to a Step.

But what's happening with that return value? Remembering the meanings for all these datatypes, it's an Iteratee which will be fed a stream of <code>aOut</code>s and return a Step (aka, a new iteratee, right?). This kind of makes intuitive sense: we've introduced a middle man which accepts input from one source and transforms a Step to a newer state.

But now perhaps the trickiest part of the whole thing: how do we actually *use* this map function? It turns out that an Enumeratee is close enough in type signature to an Enumerator that we can just do:

    map read $$ sumIter

But the type signature on *that* turns out to be a little bit weird:

    Iteratee String m (Step Int m Int)

Remembering that an Iteratee is just a wrapped up Step, what we've got *here* is an iteratee that takes Strings and returns an Iteratee, which in turn takes Ints and produces an Int. Having this fancy result allows us to do one of our great tricks with iteratees: plug in data from multiple sources. For example, we could plug some Strings into this whole ugly thing, run it, get a *new* iteratee which takes Ints, feed *that* some Ints and get an Int result.

(If all that went over your head, don't worry. I won't be talking about that kind of stuff any more.)

But often times, we *don't* need all of that power. We just want to stick our enumeratee onto our iteratee and get a new iteratee. In our case, we want to attach our map onto the sumIter to produce a new iteratee that takes Strings and returns Ints. In order to do that, we need a function like this:

    unnest :: Monad m => Iteratee String m (Step Int m Int) -> Iteratee String m Int
    unnest outer = do -- using the Monad instance of Iteratee
        inner <- outer -- inner :: Step Int m Int
        go inner
      where
        go (Error e) = throwError e
        go (Yield x _) = yield x EOF
        go (Continue k) = k EOF >>== go

We can then run our unholy mess with:

    run_ (lineEnum $$ unnest $ map read $$ sumIter) >>= print

And actually, the unnest function is available in Data.Enumerator, and it's called joinI. So we should really write:

    run_ (lineEnum $$ joinI $ map read $$ sumIter) >>= print

## Skipping

Let's write a slightly more interesting enumeratee: this one skips every other input value.

    skip :: Monad m => Enumeratee a a m b
    skip (Continue k) = do
        x <- head
        _ <- head -- the one we're skipping
        case x of
            Nothing -> return $ Continue k
            Just y -> do
                newStep <- lift $ runIteratee $ k $ Chunks [y]
                skip newStep
    skip step = return step

What's interesting about the approach here is how similar it looks to an Enumerator. We're doing a lot of the same things: checking if the Step value is a Continue; if it's not, then simply return it. Then we capitalize on the Iteratee Monad instance, using the head function to pop two values out of the stream. If there's no more data, we return the original Continue value: just like with an Enumerator, we don't give an EOF so that we can feed more data into the iteratee later. If there is data, we pass it off to the iteratee, get our new step value and then loop.

And what's cool about enumeratees is we can chain these all together:

    run_ (lineEnum $$ joinI $ skip $$ joinI $ map read $$ sumIter) >>= print

Here, we read lines, skip every other input, convert the Strings to Ints and sum them.

## Real life examples: http-enumerator package

I started working on these tutorials as I was working on the [http-enumerator](http://hackage.haskell.org/package/http-enumerator) package. I think the usage of enumeratees there is a great explanation of the benefits they can offer in real life. There are three different ways the response body can be broken up:

* Chunked encoding. In this case, the web server gives a hex string specifying the length of the next chunk and then that chunk. At the end, it sends a 0 to indicate the end of that response.

* Content length. Here, the web server sends a header before any of the body is sent specifying the total length of the body.

* Nothing at all. In this case, the response body lasts until an end-of-file.

In addition, the body may or may not be GZIP compressed. We end up with the following enumeratees, each with type signature <code>Enumeratee ByteString ByteString m b</code>: chunkedEncoding, contentLength and ungzip. We then get to do something akin to:

    let parseBody x =
            if ("transfer-encoding", "chunked") `elem` responseHeaders
                then joinI $ chunkedEncoding $$ x
                else case mlen of
                        Just len -> joinI $ contentLength len $$ x
                        Nothing -> x -- no enumeratee applied at all
    let decompress x =
            if ("content-encoding", "gzip") `elem` responseHeaders
                then joinI $ ungzip $$ x
                else x
    run_ $ socketEnumerator $$ parseBody $ decompress $ bodyIteratee

We create a chain: the data from the server is fed into the parseBody function. In the case of chunked encoding, the data is processed appropriately and then headers are filtered out. If we are dealing with content length, then only the specified number of bytes are read. And in the case of neither of those, parseBody is a no-op.

Whatever the case may be, the raw response body is then fed into decompress. If the body is GZIPed, then ungzip inflates it, otherwise decompress is a no-op. Finally, the parsed and inflated data is fed into the user-supplied bodyIteratee function. The user remains blissfully unaware of any steps the data took to get to him/her.

## Exercises

* Write an enumeratee which takes hex chars (eg, "DEADBEEF") to Word8s. Its type signature should be <code>Enumeratee Char Word8 m b</code>.

* Write the opposite enumeratee, eg <code>Enumeratee Word8 Char m b</code>.

* Create a quickcheck property that ensures that these two functions work correctly.

## Conclusion

* Enumeratees are the pipes connecting enumerators to iteratees.

* The strange type signature of an Enumeratee hides a lot of possible power. Especially notice how similar their type signatures are to Enumerators.

* You can merge an Enumeratee into an Iteratee with <code>joinI $ enumeratee $$ iteratee</code>.

* Don't forget that you can use the Monad instance of Iteratee when creating your own enumeratees.

* You can always compose multiple enumeratees together, such as in http-enumerator.

This concludes the three parts of the tutorial that I'd planned. If people had particular questions or topics they wanted me to cover, just leave a comment or send me an email.
