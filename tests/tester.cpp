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

#include <fstream>
#include <iostream>
#include <sstream>
#include <string>

#include <joelang/context.hpp>
#include <joelang/effect.hpp>
#include <joelang/state.hpp>

#include <joemath/joemath.hpp>
using namespace JoeMath;

std::string FindExpectedValues( const std::string& contents, bool& error )
{
    // Regex: /\*\s*([^\*]*?)
    enum State
    {
        Begin,   // Look for a '/'
        Slash,   // Look for a '*'
        Star,    // Read all the whitespace we can
        Content, // Read the content until the '*'
    };

    //
    // This will return the text inside the first block comment
    //
    std::string::const_iterator p = contents.begin();
    std::string ret;
    State state = Begin;
    error = false;

    while( p != contents.end() )
    {
        char c = *p++;
        switch( state )
        {
        case Begin:
            if( c == '/' )
            {
                state = Slash;
                continue;
            }
            break;
        case Slash:
            if( c == '*' )
            {
                state = Star;
                continue;
            }
            state = Begin;
            break;
        case Star:
        {
            if( std::isspace( c ) )
                continue;
            state = Content;
        }
        case Content:
        {
            if( c == '*' )
                return ret;
            ret += c;
        }
        }
    }
    error = true;
    return "";
}

std::string ReplaceNewLines( const std::string& string )
{
    // regex "\s*(\n|\r)+\s*" );

    enum State
    {
        Begin,   // Look for some whitespace otherswise append to ret
        NewLine, // Look for any more \r or \n
    };

    std::string::const_iterator p = string.begin();
    std::string ret;
    std::string match;
    State state = Begin;

    while( p != string.end() )
    {
        char c = *p++;
        switch( state )
        {
        case Begin:
            if( c == '\n' || c == '\r' )
            {
                state = NewLine;
                continue;
            }
            else if( std::isspace( c ) )
            {
                // Stay in this state but keep the spaces
                match += c;
                continue;
            }
            ret += match;
            match.clear();
            ret += c;
            break;
        case NewLine:
            if( std::isspace( c ) )
                continue;
            match.clear();
            ret += c;
            state = Begin;
            break;
        }
    }
    return ret;
}

template <typename T>
std::string GetMatrixString( T m )
{
    std::string ret;
    for( unsigned i = 0; i < T::rows; ++i )
        for( unsigned j = 0; j < T::columns; ++j )
            ret += std::to_string( m.GetRow( i )[j] ) + " ";
    return ret;
}

template <typename T>
std::string GetVectorString( T v )
{
    std::string ret;
    for( unsigned i = 0; i < T::vector_size; ++i )
        ret += std::to_string( v[i] ) + " ";
    return ret;
}

int main( int argc, char** argv )
{
    if( argc < 2 )
    {
        std::cout << "No test file given" << std::endl;
        return -1;
    }

    std::string test_filename = argv[1];
    std::ifstream in( test_filename, std::ios::in );
    if( !in )
    {
        std::cout << "Couldn't open test file" << std::endl;
        return -1;
    }

    std::string contents;
    in.seekg( 0, std::ios::end );
    contents.resize( in.tellg() );
    in.seekg( 0, std::ios::beg );
    in.read( &contents[0], contents.size() );
    in.close();

    std::stringstream actual;
    bool error;
    std::string expected = FindExpectedValues( contents, error );

    if( error )
    {
        std::cerr << "Couldn't find expected value comment" << std::endl;
        return -1;
    }

    //
    // Replace whitespace with newlines
    //
    expected = ReplaceNewLines( expected );

    JoeLang::Context context;

    context.SetErrorCallback( []( const std::string & error )->void {
        std::cerr << error << std::endl;
    } );

#define STR( s ) #s

#define CREATE_SCALAR_STATE( name, type )                     \
    JoeLang::State<type> name##_state( STR( name##_state ) ); \
    name##_state.SetCallbacks( [&actual]( type v )->void {    \
        actual << STR( name##_state ) << ": " << v << "\n";   \
    } );                                                      \
    context.AddState( &name##_state )

#define CREATE_VECTOR_STATE( name, type, rows )                                                \
    JoeLang::State<Matrix<type, ( rows ), 1>> name##rows##_state( STR( name##rows##_state ) ); \
    name##rows##_state.SetCallbacks( [&actual]( Matrix<type, ( rows ), 1> m )->void {          \
        actual << STR( name##rows##_state ) << ": " << GetVectorString( m ) << "\n";           \
    } );                                                                                       \
    context.AddState( &name##rows##_state )

#define CREATE_MATRIX_STATE( name, type, columns, rows )                                         \
    JoeLang::State<Matrix<type, ( rows ), ( columns )>> name##columns##x##rows##_state(          \
        STR( name##columns##x##rows##_state ) );                                                 \
    name##columns##x##rows##_state.SetCallbacks(                                                 \
        [&actual]( Matrix<type, ( rows ), ( columns )> m )->void {                               \
        actual << STR( name##columns##x##rows##_state ) << ": " << GetMatrixString( m ) << "\n"; \
    } );                                                                                         \
    context.AddState( &name##columns##x##rows##_state )

#define CREATE_STATE_N( name, type )         \
    CREATE_SCALAR_STATE( name, type );       \
    CREATE_VECTOR_STATE( name, type, 2 );    \
    CREATE_VECTOR_STATE( name, type, 3 );    \
    CREATE_VECTOR_STATE( name, type, 4 );    \
    CREATE_MATRIX_STATE( name, type, 2, 2 ); \
    CREATE_MATRIX_STATE( name, type, 2, 3 ); \
    CREATE_MATRIX_STATE( name, type, 2, 4 ); \
    CREATE_MATRIX_STATE( name, type, 3, 2 ); \
    CREATE_MATRIX_STATE( name, type, 3, 3 ); \
    CREATE_MATRIX_STATE( name, type, 3, 4 ); \
    CREATE_MATRIX_STATE( name, type, 4, 2 ); \
    CREATE_MATRIX_STATE( name, type, 4, 3 ); \
    CREATE_MATRIX_STATE( name, type, 4, 4 )

    CREATE_SCALAR_STATE( string, std::string );
    CREATE_STATE_N( bool, bool );
    CREATE_STATE_N( char, s8 );
    CREATE_STATE_N( short, s16 );
    CREATE_STATE_N( int, s32 );
    CREATE_STATE_N( long, s64 );
    CREATE_STATE_N( uchar, u8 );
    CREATE_STATE_N( ushort, u16 );
    CREATE_STATE_N( uint, u32 );
    CREATE_STATE_N( ulong, u64 );
    CREATE_STATE_N( float, float );
    CREATE_STATE_N( double, double );

#undef CREATE_STATE_N
#undef CREATE_SCALAR_STATE
#undef CREATE_VECTOR_STATE
#undef CREATE_MATRIX_STATE
#undef STR

    // TODO make this reuse the string when we can load from string
    JoeLang::Effect* e = context.CreateEffectFromString( contents );

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

    std::string actual_string = ReplaceNewLines( actual.str() );
    if( actual_string != expected )
    {
        std::cout << "Mismatch between expected and actual result\n"
                  << "Expected: \"" << expected << "\"\n"
                  << "Actual:   \"" << actual_string << "\"" << std::endl;
        return -1;
    }

    return 0;
}
