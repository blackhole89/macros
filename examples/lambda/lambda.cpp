#include <stdio.h>

@global $counter (0)
@global $lambda_bodies @[]

/* This generates a unique name for the lambda, and pushes its function body
 * into $lambda_bodies to be emitted at the chosen top-level point. */
@define lambda {
    ( (@^$args) -> @^[{}]$type $body ) => (
        @set $counter (@calc($counter+1))
        @push_back $lambda_bodies (
            $type lam@@$counter ($args)
            $body
        )
        lam@@$counter 
    )
}

/* This macro captures everything that follows it. 
 * Since unparenthesised (open-ended) macro arguments are not expanded before
 * matching, we need to explicitly process (@eval) the token stream for lambdas.
 * */
@define top_level {
    ( @^$everything ) => (
        // the following forces evaluation of all macros
        @set $everything (@eval ($everything)) 
        @for($l : $lambda_bodies)($l)
        $everything
    )
}

top_level;

int main(int argc, char* argv[])
{
    auto a = lambda (int a, int b) -> int { return a+b; };
    auto b = lambda (int c) -> int { return -c; };

    printf("a(3,5)=%d; b(4)=%d\n",a(3,5),b(4));
    
    return 0;
}

