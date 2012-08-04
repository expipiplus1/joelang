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

#include "literal_expression.hpp"

#include <cassert>
#include <iostream>
#include <limits>
#include <memory>
#include <sstream>
#include <string>
#include <utility>

#include <engine/types.hpp>
#include <compiler/type_properties.hpp>
#include <compiler/code_generator.hpp>
#include <compiler/generic_value.hpp>
#include <compiler/parser.hpp>
#include <compiler/sema_analyzer.hpp>
#include <compiler/terminal_types.hpp>
#include <compiler/tokens/token.hpp>

namespace JoeLang
{
namespace Compiler
{

//------------------------------------------------------------------------------
// LiteralExpression
//------------------------------------------------------------------------------

LiteralExpression::LiteralExpression( TokenTy sub_class_id )
    :Expression( sub_class_id )
{
}

bool LiteralExpression::ResolveIdentifiers( SemaAnalyzer& sema )
{
    return true;
}

bool LiteralExpression::PerformSema( SemaAnalyzer& sema )
{
    return true;
}

Type LiteralExpression::GetUnderlyingType() const
{
    return GetReturnType();
}

const ArrayExtents& LiteralExpression::GetArrayExtents() const
{
    const static ArrayExtents empty;
    return empty;
}

bool LiteralExpression::IsConst() const
{
    return true;
}

bool LiteralExpression::Parse( Parser& parser,
                               Expression_up& token )
{
    // Try to parse any of the literals
    std::unique_ptr<Token> t;
    if( !parser.ExpectAnyOf<FloatingLiteralExpression,
                            IntegerLiteralExpression,
                            BooleanLiteralExpression,
                            CharacterLiteralExpression,
                            StringLiteralExpression>( t ) )
        return false;

    // Cast to a LiteralExpression
    token.reset( static_cast<LiteralExpression*>( t.release() ) );
    return true;
}

std::unique_ptr<LiteralExpression> LiteralExpression::Create(
                                                         const GenericValue& v )
{
    switch( v.GetUnderlyingType() )
    {
    case Type::BOOL:
        return std::unique_ptr<LiteralExpression>(
                                new BooleanLiteralExpression( v.GetBool() ) );
    case Type::I8:
        return std::unique_ptr<LiteralExpression>( new IntegerLiteralExpression(
                                 v.GetI8(),
                                 IntegerLiteralExpression::Suffix::CHAR ) );
    case Type::I16:
        return std::unique_ptr<LiteralExpression>( new IntegerLiteralExpression(
                                 v.GetI16(),
                                 IntegerLiteralExpression::Suffix::SHORT ) );
    case Type::I32:
        return std::unique_ptr<LiteralExpression>( new IntegerLiteralExpression(
                                 v.GetI32(),
                                 IntegerLiteralExpression::Suffix::INT ) );
    case Type::I64:
        return std::unique_ptr<LiteralExpression>( new IntegerLiteralExpression(
                                 v.GetI64(),
                                 IntegerLiteralExpression::Suffix::LONG ) );
    case Type::U8:
        return std::unique_ptr<LiteralExpression>( new IntegerLiteralExpression(
                           v.GetU8(),
                           IntegerLiteralExpression::Suffix::UNSIGNED_CHAR ) );
    case Type::U16:
        return std::unique_ptr<LiteralExpression>( new IntegerLiteralExpression(
                           v.GetU16(),
                           IntegerLiteralExpression::Suffix::UNSIGNED_SHORT ) );
    case Type::U32:
        return std::unique_ptr<LiteralExpression>( new IntegerLiteralExpression(
                           v.GetU32(),
                           IntegerLiteralExpression::Suffix::UNSIGNED_INT ) );
    case Type::U64:
        return std::unique_ptr<LiteralExpression>( new IntegerLiteralExpression(
                           v.GetU64(),
                           IntegerLiteralExpression::Suffix::UNSIGNED_LONG ) );
    case Type::FLOAT:
        return std::unique_ptr<LiteralExpression>(new FloatingLiteralExpression(
                                v.GetFloat(),
                                FloatingLiteralExpression::Suffix::SINGLE ) );
    case Type::DOUBLE:
        return std::unique_ptr<LiteralExpression>(new FloatingLiteralExpression(
                                v.GetDouble(),
                                FloatingLiteralExpression::Suffix::NONE ) );
    case Type::STRING:
        return std::unique_ptr<LiteralExpression>( new StringLiteralExpression(
                                                            v.GetString() ) );
    default:
        assert( false &&
                "Trying to create LiteralExpression with an unhandled type" );
    }
    return nullptr;
}

bool LiteralExpression::classof( const Expression* e )
{
    return e->GetSubClassID() >= TokenTy::LiteralExpression_Start &&
           e->GetSubClassID() <= TokenTy::LiteralExpression_End;
}

bool LiteralExpression::classof( const LiteralExpression* e )
{
    return true;
}

//------------------------------------------------------------------------------
// IntegerlLiteralExpression
//------------------------------------------------------------------------------

IntegerLiteralExpression::IntegerLiteralExpression( jl_u64 value,
                                                    Suffix suffix )
    :LiteralExpression( TokenTy::IntegerLiteralExpression )
    ,m_Value( value )
    ,m_Suffix( suffix )
{
}

IntegerLiteralExpression::~IntegerLiteralExpression()
{
}

llvm::Value* IntegerLiteralExpression::CodeGen( CodeGenerator& code_gen ) const
{
    return code_gen.CreateInteger( m_Value, GetReturnType() );
}

CompleteType IntegerLiteralExpression::GetType() const
{
    Type type;
    switch( m_Suffix )
    {
    case Suffix::CHAR:
        type = Type::I8;
        break;
    case Suffix::INT:
        type = Type::I32;
        break;
    case Suffix::SHORT:
        type = Type::I16;
        break;
    case Suffix::LONG:
        type = Type::I64;
        break;
    case Suffix::UNSIGNED_CHAR:
        type = Type::U8;
        break;
    case Suffix::UNSIGNED_INT:
        type = Type::U32;
        break;
    case Suffix::UNSIGNED_SHORT:
        type = Type::U16;
        break;
    case Suffix::UNSIGNED_LONG:
        type = Type::U64;
        break;
    default:
        type = m_Value <= jl_u64(std::numeric_limits<jl_i32>::max())
                    ? Type::I32
                    : m_Value <= jl_u64(std::numeric_limits<jl_u32>::max())
                        ? Type::U32
                        : m_Value <= jl_u64(std::numeric_limits<jl_i64>::max())
                            ? Type::I64
                            : Type::U64;
        break;
    }

    return CompleteType( type );
}

Type IntegerLiteralExpression::GetReturnType() const
{
    switch( m_Suffix )
    {
    case Suffix::CHAR:
        return Type::I8;
    case Suffix::INT:
        return Type::I32;
    case Suffix::SHORT:
        return Type::I16;
    case Suffix::LONG:
        return Type::I64;
    case Suffix::UNSIGNED_CHAR:
        return Type::U8;
    case Suffix::UNSIGNED_INT:
        return Type::U32;
    case Suffix::UNSIGNED_SHORT:
        return Type::U16;
    case Suffix::UNSIGNED_LONG:
        return Type::U64;
    default:
        return m_Value <= jl_u64(std::numeric_limits<jl_i32>::max())
                    ? Type::I32
                    : m_Value <= jl_u64(std::numeric_limits<jl_u32>::max())
                        ? Type::U32
                        : m_Value <= jl_u64(std::numeric_limits<jl_i64>::max())
                            ? Type::I64
                            : Type::U64;
    }
}

GenericValue IntegerLiteralExpression::GetValue() const
{
    switch( m_Suffix )
    {
    case Suffix::CHAR:
        return GenericValue( jl_i8(m_Value) );
    case Suffix::INT:
        return GenericValue( jl_i32(m_Value) );
    case Suffix::SHORT:
        return GenericValue( jl_i16(m_Value) );
    case Suffix::LONG:
        return GenericValue( jl_i64(m_Value) );
    case Suffix::UNSIGNED_CHAR:
        return GenericValue( jl_u8(m_Value) );
    case Suffix::UNSIGNED_INT:
        return GenericValue( jl_u32(m_Value) );
    case Suffix::UNSIGNED_SHORT:
        return GenericValue( jl_u16(m_Value) );
    case Suffix::UNSIGNED_LONG:
        return GenericValue( jl_u64(m_Value) );
    default:
        return m_Value <= jl_u64(std::numeric_limits<jl_i32>::max())
                    ? GenericValue( jl_i32(m_Value) )
                    : m_Value <= jl_u64(std::numeric_limits<jl_u32>::max())
                        ? GenericValue( jl_u32(m_Value) )
                        : m_Value <= jl_u64(std::numeric_limits<jl_i64>::max())
                            ? GenericValue( jl_i64(m_Value) )
                            : GenericValue( jl_u64(m_Value) );
    }
}

void IntegerLiteralExpression::Print( int depth ) const
{
    for( int i = 0; i < depth * 4; ++i )
        std::cout << " ";
    std::cout << m_Value << "\n";
}

bool IntegerLiteralExpression::ParseInteger( std::string string,
                                             jl_u64& value,
                                             Suffix& suffix )
{
    if( string.empty() )
        return false;

    std::istringstream ss;
    ss.unsetf( std::ios_base::skipws );

    // If this is a Hex or Oct constant
    if( string[0] == '0' )
    {
        // If this is a Hex constant
        if( string.size() > 2 &&
            ( string[1] == 'x' || string[1] == 'X' ) )
        {
            // Initialize a stringstream with the hex part of this string
            // Read as hexadecimal
            ss.str( string.substr(2) );
            ss.setf( std::ios_base::hex, std::ios::basefield );
            if( !( ss >> value ) )
                return false;
        }
        else
        {
            // Read as octal
            ss.str( string );
            ss.setf( std::ios_base::oct, std::ios::basefield );
            if( !( ss >> value ) )
                return false;
        }
    }
    else
    {
        // Read as decimal
        ss.str( string );
        ss.setf( std::ios_base::dec, std::ios::basefield );
        if( !( ss >> value ) )
            return false;
    }

    // check if there is a suffix
    if( ss.eof() )
    {
        suffix = Suffix::NONE;
        return true;
    }

    // There is a suffix
    if( string[ss.tellg()] == 'u' )
    {
        if( string.size() + 1 == std::size_t(ss.tellg()) )
        {
            if( string[ss.tellg()] == 'i' )
                suffix = Suffix::UNSIGNED_INT;
            else if( string[ss.tellg()] == 'l' )
                suffix = Suffix::UNSIGNED_LONG;
            else if( string[ss.tellg()] == 's' )
                suffix = Suffix::UNSIGNED_SHORT;
            else if( string[ss.tellg()] == 't' )
                suffix = Suffix::UNSIGNED_CHAR;
            else
                return false;
        }
        else
            suffix = Suffix::UNSIGNED;
    }
    else if( string[ss.tellg()] == 'i' )
        suffix = Suffix::INT;
    else if( string[ss.tellg()] == 'l' )
        suffix = Suffix::LONG;
    else if( string[ss.tellg()] == 's' )
        suffix = Suffix::SHORT;
    else if( string[ss.tellg()] == 't' )
        suffix = Suffix::CHAR;
    else
        return false;

    return true;
}

bool IntegerLiteralExpression::Parse(
                            Parser& parser,
                            std::unique_ptr<IntegerLiteralExpression>& token )
{
    std::string string;
    if( !parser.ExpectTerminal( TerminalType::INTEGER_LITERAL, string ) )
        return false;

    jl_u64 value;
    Suffix suffix;

    if( !ParseInteger( string, value, suffix ) )
        return false;

    token.reset( new IntegerLiteralExpression( value, suffix ) );
    return true;
}

bool IntegerLiteralExpression::classof( const Expression* e )
{
    return e->GetSubClassID() == TokenTy::IntegerLiteralExpression;
}

bool IntegerLiteralExpression::classof( const IntegerLiteralExpression* e )
{
    return true;
}

//------------------------------------------------------------------------------
// FloatingLiteralExpression
//------------------------------------------------------------------------------

FloatingLiteralExpression::FloatingLiteralExpression( double value,
                                                      Suffix suffix )
    :LiteralExpression( TokenTy::FloatingLiteralExpression )
    ,m_Value( value )
    ,m_Suffix( suffix )
{
}

FloatingLiteralExpression::~FloatingLiteralExpression()
{
}

llvm::Value* FloatingLiteralExpression::CodeGen( CodeGenerator& code_gen ) const
{
    return code_gen.CreateFloating( m_Value,
                                    m_Suffix == Suffix::SINGLE ? Type::FLOAT
                                                               : Type::DOUBLE );
}

CompleteType FloatingLiteralExpression::GetType() const
{
    if( m_Suffix == Suffix::SINGLE )
        return CompleteType( Type::FLOAT );
    else
        return CompleteType( Type::DOUBLE );
}

Type FloatingLiteralExpression::GetReturnType() const
{
    if( m_Suffix == Suffix::SINGLE )
        return Type::FLOAT;
    else
        return Type::DOUBLE;
}

GenericValue FloatingLiteralExpression::GetValue() const
{
    switch( m_Suffix )
    {
    case Suffix::SINGLE:
        return GenericValue( jl_float(m_Value) );
    default:
        return GenericValue( jl_double(m_Value) );
    }
}

void FloatingLiteralExpression::Print( int depth ) const
{
    for( int i = 0; i < depth * 4; ++i )
        std::cout << " ";
    std::cout << m_Value << "\n";
}

bool FloatingLiteralExpression::ParseFloat( std::string string,
                                            double& value,
                                            Suffix& suffix )
{
    std::istringstream ss( string );
    ss.unsetf( std::ios_base::skipws );
    if( !( ss >> value ) )
        return false;

    // check if there is a suffix
    if( ss.eof() )
    {
        suffix = Suffix::NONE;
        return true;
    }

    // There is a suffix
    if( string[ss.tellg()] == 'f' )
        suffix = Suffix::SINGLE;
    else
        return false;

    return true;
}

bool FloatingLiteralExpression::Parse(
                            Parser& parser,
                            std::unique_ptr<FloatingLiteralExpression>& token )
{
    // Parse a floating literal
    std::string string;
    if( !parser.ExpectTerminal( TerminalType::FLOATING_LITERAL, string ) )
        return false;

    double value;
    Suffix suffix;
    if( !ParseFloat( string, value, suffix ) )
        return false;

    token.reset( new FloatingLiteralExpression( value, suffix ) );
    return true;
}

bool FloatingLiteralExpression::classof( const Expression* e )
{
    return e->GetSubClassID() == TokenTy::FloatingLiteralExpression;
}

bool FloatingLiteralExpression::classof( const FloatingLiteralExpression* e )
{
    return true;
}

//------------------------------------------------------------------------------
// BooleanLiteralExpression
//------------------------------------------------------------------------------

BooleanLiteralExpression::BooleanLiteralExpression( bool value )
    :LiteralExpression( TokenTy::BooleanLiteralExpression )
    ,m_Value( value )
{
}

BooleanLiteralExpression::~BooleanLiteralExpression()
{
}

llvm::Value* BooleanLiteralExpression::CodeGen( CodeGenerator& code_gen ) const
{
    return code_gen.CreateInteger( m_Value, Type::BOOL );
}

CompleteType BooleanLiteralExpression::GetType() const
{
    return CompleteType( Type::BOOL );
}

Type BooleanLiteralExpression::GetReturnType() const
{
    return Type::BOOL;
}

GenericValue BooleanLiteralExpression::GetValue() const
{
    return GenericValue( jl_bool(m_Value) );
}

void BooleanLiteralExpression::Print( int depth ) const
{
    for( int i = 0; i < depth * 4; ++i )
        std::cout << " ";
    std::cout << std::boolalpha << m_Value << std::noboolalpha << "\n";
}

bool BooleanLiteralExpression::Parse(
                            Parser& parser,
                            std::unique_ptr<BooleanLiteralExpression>& token )
{
    // Try to parse 'true'
    if( parser.ExpectTerminal( TerminalType::TRUE ) )
    {
        token.reset( new BooleanLiteralExpression( true ) );
        return true;
    }

    // Try to parse 'false'
    if( parser.ExpectTerminal( TerminalType::FALSE ) )
    {
        token.reset( new BooleanLiteralExpression( false ) );
        return true;
    }

    return false;
}

bool BooleanLiteralExpression::classof( const Expression* e )
{
    return e->GetSubClassID() == TokenTy::BooleanLiteralExpression;
}

bool BooleanLiteralExpression::classof( const BooleanLiteralExpression* e )
{
    return true;
}

/**
  * This function unescapes a single character
  * It's used by StringLiteralExpression and CharacterLiteralExpression
  * \param character
  *   the char to unescape
  * \returns the unescaped character
  */
char UnescapeCharacter( char c )
{
    char ret;
    switch( c )
    {
    case '\'':
        ret = '\'';
        break;
    case '\"':
        ret = '\"';
        break;
    case '\?':
        ret = '\?';
        break;
    case '\\':
        ret = '\\';
        break;
    case '\a':
        ret = '\a';
        break;
    case '\b':
        ret = '\b';
        break;
    case '\f':
        ret = '\f';
        break;
    case '\n':
        ret = '\n';
        break;
    case '\r':
        ret = '\r';
        break;
    case '\t':
        ret = '\t';
        break;
    case '\v':
        ret = '\v';
        break;
    default:
        //TODO warning here
        ret = c;
    }

    return ret;
}

//------------------------------------------------------------------------------
// StringLiteralExpression
//------------------------------------------------------------------------------

StringLiteralExpression::StringLiteralExpression( std::string value )
    :LiteralExpression( TokenTy::BooleanLiteralExpression )
    ,m_Value( std::move(value) )
{
}

StringLiteralExpression::~StringLiteralExpression()
{
}

llvm::Value* StringLiteralExpression::CodeGen( CodeGenerator& code_gen ) const
{
    return code_gen.CreateString( m_Value );
}

CompleteType StringLiteralExpression::GetType() const
{
    return CompleteType( Type::STRING );
}

Type StringLiteralExpression::GetReturnType() const
{
    return Type::STRING;
}

GenericValue StringLiteralExpression::GetValue() const
{
    return GenericValue( m_Value );
}

void StringLiteralExpression::Print( int depth ) const
{
    for( int i = 0; i < depth * 4; ++i )
        std::cout << " ";
    std::cout << '\"' << m_Value << "\"\n";
}

const std::string& StringLiteralExpression::GetString() const
{
    return m_Value;
}

bool StringLiteralExpression::Parse(
                            Parser& parser,
                            std::unique_ptr<StringLiteralExpression>& token )
{
    // Parse the escaped string literal into string
    std::string string;
    if( !parser.ExpectTerminal( TerminalType::STRING_LITERAL, string ) )
        return false;

    // Unescape the string
    std::string unescaped_string;
    if( !UnquoteAndUnescapeString( string, unescaped_string ) )
        return false;

    token.reset( new StringLiteralExpression( std::move(unescaped_string) ) );
    return true;
}

bool StringLiteralExpression::UnquoteAndUnescapeString(
                                                const std::string& string,
                                                std::string& unescaped_string )
{
    if( string.size() < 2 )
        return false;
    if( *string.begin() != '\"' )
        return false;
    if( *string.rbegin() != '\"' )
        return false;

    // reserve space for a string with no escaped characters
    unescaped_string.clear();
    unescaped_string.reserve( string.size() - 2 );

    std::string::const_iterator p = string.begin() + 1;
    while( p != string.end() - 1 )
    {
        if( *p == '\\' )
        {
            ++p;
            unescaped_string += UnescapeCharacter( *p );
        }
        unescaped_string += *p;
        ++p;
    }
    return true;
}

bool StringLiteralExpression::classof( const Expression* e )
{
    return e->GetSubClassID() == TokenTy::StringLiteralExpression;
}

bool StringLiteralExpression::classof( const StringLiteralExpression* e )
{
    return true;
}

//------------------------------------------------------------------------------
// CharacterLiteralExpression
//------------------------------------------------------------------------------

CharacterLiteralExpression::CharacterLiteralExpression( char value )
    :LiteralExpression( TokenTy::CharacterLiteralExpression )
    ,m_Value( value )
{
}

CharacterLiteralExpression::~CharacterLiteralExpression()
{
}

llvm::Value* CharacterLiteralExpression::CodeGen(
                                                CodeGenerator& code_gen ) const
{
    return code_gen.CreateInteger( m_Value, Type::I8 );
}

CompleteType CharacterLiteralExpression::GetType() const
{
    return CompleteType( Type::I8 );
}

Type CharacterLiteralExpression::GetReturnType() const
{
    return Type::I8;
}

GenericValue CharacterLiteralExpression::GetValue() const
{
    return GenericValue( jl_i8(m_Value) );
}

void CharacterLiteralExpression::Print( int depth ) const
{
    for( int i = 0; i < depth * 4; ++i )
        std::cout << " ";
    std::cout << '\'' << m_Value << '\'' << "\n";
}

bool CharacterLiteralExpression::Parse(
                            Parser& parser,
                            std::unique_ptr<CharacterLiteralExpression>& token )
{
    // Parse the escaped char into string
    std::string string;
    if( !parser.ExpectTerminal( TerminalType::CHARACTER_LITERAL, string ) )
        return false;

    char unescaped_char;
    if( !UnquoteAndUnescapeChar( string, unescaped_char ) )
        return false;

    token.reset( new CharacterLiteralExpression( unescaped_char ) );
    return true;
}

bool CharacterLiteralExpression::UnquoteAndUnescapeChar(
                                                const std::string& string,
                                                char& unescaped_char )
{
    if( string.size() < 3 )

    if( string[1] == '\\' )
    {
        if( string.size() != 4 )
            return false;
        unescaped_char = UnescapeCharacter( string[2] );
        return true;
    }

    unescaped_char = UnescapeCharacter( string[1] );
    return true;
}

bool CharacterLiteralExpression::classof( const Expression* e )
{
    return e->GetSubClassID() == TokenTy::CharacterLiteralExpression;
}

bool CharacterLiteralExpression::classof( const CharacterLiteralExpression* e )
{
    return true;
}

} // namespace Compiler
} // namespace JoeLang
