/*
    Copyright 2011 Joe Hermaszewski. All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions are met:

    1. Redistributions of source code must retain the above copyright notice,
    this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright notice,
    this list of conditions and the following disclaimer in the documentation
    and/or other materials provided with the distribution.

    THIS SOFTWARE IS PROVIDED BY JOE HERMASZEWSKI "AS IS" AND ANY EXPRESS OR
    IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
    MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
    EVENT SHALL JOE HERMASZEWSKI OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
    INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
    (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
    ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
    THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

    The views and conclusions contained in the software and documentation are
    those of the authors and should not be interpreted as representing official
    policies, either expressed or implied, of Joe Hermaszewski.
*/

#include <iostream>
#include <string>

#include <joelang/context.hpp>
#include <joelang/effect.hpp>
#include <joelang/state.hpp>

#include <joemath/joemath.hpp>

int main( int argc, char** argv )
{
    JoeLang::Context context;

    JoeLang::State<int> my_int_state( "my_int_state" );
    my_int_state.SetCallbacks( [](int v) -> void
                                 {std::cout << "setting my_int_state to "
                                            << v << std::endl;} );

    JoeLang::State<float> my_state( "my_floating_state",
                                    std::map<std::string,float>({{"one", 1}, 
                                                                 {"two", 2}}) );

    my_state.SetCallbacks( [](float v) -> void
                             {std::cout << "setting my_floating_state to "
                                        << v << std::endl;} );

    JoeLang::State<bool> my_bool_state(
                                "my_bool_state",
                                std::map<std::string,bool>({{"on",true},
                                                            {"off", false}}) );
    my_bool_state.SetCallbacks( [](bool v) -> void
                                  {std::cout << "setting my_bool_state to "
                                             << v << std::endl;} );

    JoeLang::State<std::string> my_string_state(
                    "my_string_state",
                    std::map<std::string, std::string>({{"string_enumerant",
                                                         "string_enumerant"}}));
    my_string_state.SetCallbacks( [](std::string v) -> void
                                  {std::cout << "setting my_string_state to "
                                             << v << std::endl;} );

    JoeLang::State<JoeMath::float4> my_float4_state(
      "my_float4_state",
      std::map<std::string,JoeMath::float4>({{"ones", JoeMath::float4{1,1,1,1}},
                                             {"zeroes", JoeMath::float4(0)}}) );
    my_float4_state.SetCallbacks( [](JoeMath::float4 v) -> void
                                  {std::cout << "setting my_float4_state to "
                                             << v.x() << " " << v.y() << " "
                                             << v.z() << " " << v.w()
                                             << std::endl;} );

    context.AddState( &my_state );
    context.AddState( &my_int_state );
    context.AddState( &my_bool_state );
    context.AddState( &my_string_state );
    context.AddState( &my_float4_state );

    JoeLang::Effect* e = context.CreateEffectFromFile( "test.jfx" );

    //
    // Flush std::cerr (it's used for dumping the module)
    //
    std::cerr.flush();
    if( e )
    {
        const JoeLang::Technique* t = e->GetNamedTechnique( "t1" );
        if( t )
            for( const auto& pass : t->GetPasses() )
            {
                std::cout << "Setting Pass: \'"
                          << pass.GetName() << "\' state\n";
                pass.SetState();
                pass.ResetState();
            }
    }
    else
        std::cout << "fail\n";
}
