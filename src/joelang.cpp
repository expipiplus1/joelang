/*
    Copyright 2011 Joe Hermaszewski. All rights reserved.

    Redistribution and use in source and binary forms, with or without modification, are
    permitted provided that the following conditions are met:

    1. Redistributions of source code must retain the above copyright notice, this list of
    conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright notice, this list
    of conditions and the following disclaimer in the documentation and/or other materials
    provided with the distribution.

    THIS SOFTWARE IS PROVIDED BY JOE HERMASZEWSKI "AS IS" AND ANY EXPRESS OR IMPLIED
    WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
    FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL JOE HERMASZEWSKI OR
    CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
    CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
    SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
    ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
    NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
    ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

    The views and conclusions contained in the software and documentation are those of the
    authors and should not be interpreted as representing official policies, either expressed
    or implied, of Joe Hermaszewski.
*/

#include <iostream>

#include <engine/context.hpp>
#include <engine/effect.hpp>
#include <engine/state.hpp>

int main( int argc, char** argv )
{
    JoeLang::Context context;
    JoeLang::State my_state( "my_state", {{"one", 1}, {"two", 2}} );
    my_state.SetCallbacks( [](int v)
                             {std::cout << "setting my_state to " << v << std::endl;},
                           nullptr, nullptr );

    context.AddState( my_state );

    JoeLang::Effect* e = context.CreateEffectFromString(
                             "technique t{ pass p{ my_state=-1; } }" );

    if( e )
    {
        const JoeLang::Technique* t = e->GetNamedTechnique( "t" );
        if( t )
            for( const auto& pass : t->GetPasses() )
            {
                std::cout << "Setting Pass: \'" << pass.GetName() << "\' state\n";
                pass.SetState();
            }
    }
    else
        std::cout << "fail\n";
}
