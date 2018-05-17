#include <stdio.h>

/* Partial function application. 
 * e.g. partial f(?,x,?,?)(a,b,c) == f(a,x,b,c) */
/* This uses C++14 generic lambdas; otherwise, we'd have to
 * add some mechanism for extracting the type of each argument
 * of the partially applied function. */
@define partial {
    ( $name ( @*[,]( @^[,]$args ) ) ) => (
        @var $counter (0)
        @var $real_args @[]
        @var $lambda_args @[]
        /* Iterate over arguments, and check whether they are a hole (?)
         * or not. */
        @for( $a : $args )( 
            @match($a) {
                (?) => (
                    /* Hole. We will obtain what to plug into it as parameter
                     * p0,p1,... to the lambda we output. */
                    @push_back $real_args (p@@$counter)
                    @push_back $lambda_args (p@@$counter)
                    @set $counter (@calc($counter+1))
                )
                () => (
                    /* Not a hole. */
                    @push_back $real_args ($a)
                )
            }
        )

        /* Generate a lambda that takes in the missing parameters 
         * and calls the function. 
         * Capture closure by value in case the thing we 
         * are calling is already a callable object or the like. */
        [=]( @for($a:$lambda_args)[,](auto $a) ) { 
            return $name ( @for($a:$real_args)[,]($a) );
        }
    )
}

int sum3(int a, int b, int c)
{
    printf("Arguments: %d, %d, %d\n",a,b,c);
    return a+b+c;
}

int main(int argc, char* argv[])
{
    auto a = partial sum3(?,5,?);
    auto b = partial a(?,2);

    printf("Sum: %d\n",b(3));
    return 0;
}

