INJECTABLES
===========

**They're like global variables... Only worse.**

Injectable variables are a tool for making repl based interaction with realtime code
a little more pleasent.

Say you have the following function that is called every loop:

    (let ((model (make-model)))
      (defun render (position)
        (opengl-draw model position)))
  
If I want to access to the variable model I can change the code above to this:

    (let ((model (make-model)))
      (defun render (position)
        (opengl-draw #^model position)))
      
and then from the repl write:

    cl-user> #@model

Then the result will be the value of model from inside the function

We can also setf the injected variable, this will set it's value until we flush it.

USE
---

    #^var-name will return the injected value if there is one or will use the value of var-name
    in that scope.

    #@var-name will return the injected value if there is one or will return nil.

    Both of these can be *setf*'d
    
    (flush 'var-name) will clear the injected value
    
    (flush-all) will clear all injected values

    
INJECTABLE VARIABLES ARE CONSIDERED HARMFUL
-------------------------------------------

I know I'm going a little overboard with the warnings here but seriously, these are not for use
in production code. This is probably the wrong answer to the issue I have but for now it works
well enough.

I will be coming back to this to make it better.
