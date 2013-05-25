/*
    Copyright 2013 Joe Hermaszewski. All rights reserved.

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
#include <fstream>
#include <regex>
#include <sstream>
#include <string>

#include <joelang/context.hpp>
#include <joelang/effect.hpp>
#include <joelang/state.hpp>

#include <joemath/joemath.hpp>

int main( int argc, char** argv )
{
    if( argc < 2 )
    {
        std::cout << "No test file given" << std::endl;
        return -1;
    }

    std::string test_filename = argv[1];
    std::ifstream in(test_filename, std::ios::in);
    if( !in )
    {
        std::cout << "Couldn't open test file" << std::endl;
        return -1;
    }

    std::string contents;
    in.seekg(0, std::ios::end);
    contents.resize(in.tellg());
    in.seekg(0, std::ios::beg);
    in.read(&contents[0], contents.size());
    in.close();

    std::regex r( "/\\*\\s*([^]*?)\\*/" );
    std::smatch m;

    std::stringstream actual;
    std::string expected;

    if( !std::regex_search( contents,
                            m,
                            r,
                            std::regex_constants::match_continuous ) )
    {
        std::cout << "Couldn't find expected value comment" << std::endl;
        return -1;
    }
    else
    {
        expected = m[1];
        std::regex n( "\\s*(\n|\r)+" );
        expected = std::regex_replace( expected, n, "\n" );
    }
    
    JoeLang::Context context;

    context.SetErrorCallback( [](const std::string& error) -> void
                              { std::cerr << error << std::endl; } );

    JoeLang::State<int> int_state( "int_state" );
    int_state.SetCallbacks( [&actual](int v) -> void
                            {actual << "int_state: " << v << "\n";} );

    JoeLang::State<float> float_state( "float_state" );
    float_state.SetCallbacks( [&actual](float v) -> void
                              {actual << "float_state: " << v << "\n";} );

    JoeLang::State<bool> bool_state( "bool_state" );
    bool_state.SetCallbacks( [&actual](bool v) -> void
                             {actual << "bool_state: " << v << "\n";} );

    JoeLang::State<std::string> string_state( "string_state" );
    string_state.SetCallbacks( [&actual](std::string v) -> void
                               {actual << "string_state: " << v << "\n";} );

    JoeLang::State<JoeMath::float2> float2_state( "float2_state" );
    float2_state.SetCallbacks( [&actual](JoeMath::float2 v) -> void
                               {actual << "float2_state: "
                                       << v.x() << " " << v.y() << "\n";} );

    JoeLang::State<JoeMath::float3> float3_state( "float3_state" );
    float3_state.SetCallbacks( [&actual](JoeMath::float3 v) -> void
                               {actual << "float3_state: "
                                       << v.x() << " " << v.y() << " "
                                       << v.z() << "\n";} );

    JoeLang::State<JoeMath::float4> float4_state( "float4_state" );
    float4_state.SetCallbacks( [&actual](JoeMath::float4 v) -> void
                               {actual << "float4_state: "
                                       << v.x() << " " << v.y() << " "
                                       << v.z() << " " << v.w() << "\n";} );

    JoeLang::State<JoeMath::float4x4> float4x4_state( "float4x4_state" );
    float4x4_state.SetCallbacks( [&actual](JoeMath::float4x4 m)
                              {actual << "float4x4_state: "
                                      << m[0][0] << " " << m[1][0] << " "
                                      << m[2][0] << " " << m[3][0] << " "
                                      << m[0][1] << " " << m[1][1] << " "
                                      << m[2][1] << " " << m[3][1] << " "
                                      << m[0][2] << " " << m[1][2] << " "
                                      << m[2][2] << " " << m[3][2] << " "
                                      << m[0][3] << " " << m[1][3] << " "
                                      << m[2][3] << " " << m[3][3] << "\n";} );

    context.AddState( &int_state );
    context.AddState( &float_state );
    context.AddState( &bool_state );
    context.AddState( &string_state );
    context.AddState( &float2_state );
    context.AddState( &float3_state );
    context.AddState( &float4_state );
    context.AddState( &float4x4_state );

    // TODO make this reuse the string when we can load from string
    JoeLang::Effect* e = context.CreateEffectFromFile( test_filename );

    if( !e )
    {
        std::cout << "Error compiling effect" << std::endl;
        return -1;
    }

    for( const auto& technique : e->GetTechniques() )
        for( const auto& pass : technique.GetPasses() )
        {
            pass.SetState();
            pass.ResetState();
        }

    if( actual.str() != expected )
    {
        std::cout << "Mismatch between expected and actual result\n"
                  << "Expected: \"" << expected << "\"\n"
                  << "Actual:   \"" << actual.str() << "\"" << std::endl;
        return -1;
    }

    return 0;
}
