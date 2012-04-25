/*
    Copyright 2012 Joe Hermaszewski. All rights reserved.

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

#include "declaration_specifier.hpp"

#include <memory>

#include <compiler/parser.hpp>
#include <compiler/terminal_types.hpp>
#include <compiler/tokens/token.hpp>

namespace JoeLang
{
namespace Compiler
{

//------------------------------------------------------------------------------
// DeclarationSpecifier
//------------------------------------------------------------------------------

DeclarationSpecifier::DeclarationSpecifier()
{
}

DeclarationSpecifier::~DeclarationSpecifier()
{
}

void DeclarationSpecifier::Print( int depth ) const
{
}

bool DeclarationSpecifier::Parse( Parser& parser,
                                  std::unique_ptr<DeclarationSpecifier>& token )
{
    std::unique_ptr<Token> t;
    if( !parser.ExpectAnyOf<TypeSpecifier,
                            TypeQualifier,
                            StorageClassSpecifier>( t ) )
        return false;

    token.reset( static_cast<DeclarationSpecifier*>( t.release() ) );
    return true;
}

//------------------------------------------------------------------------------
// TypeSpecifier
//------------------------------------------------------------------------------

TypeSpecifier::TypeSpecifier( TypeSpec t )
    :m_typeSpec( t )
{
}

TypeSpecifier::~TypeSpecifier()
{
}

void TypeSpecifier::Print( int depth ) const
{
}

bool TypeSpecifier::Parse( Parser& parser,
                           std::unique_ptr<TypeSpecifier>& token )
{
    const static std::vector<std::pair<TerminalType, TypeSpec> > type_map =
    {
        { TerminalType::TYPE_VOID,     TypeSpec::VOID     },
        { TerminalType::TYPE_BOOL,     TypeSpec::BOOL     },
        { TerminalType::TYPE_CHAR,     TypeSpec::CHAR     },
        { TerminalType::TYPE_SHORT,    TypeSpec::SHORT    },
        { TerminalType::TYPE_INT,      TypeSpec::INT      },
        { TerminalType::TYPE_LONG,     TypeSpec::LONG     },
        { TerminalType::TYPE_SIGNED,   TypeSpec::SIGNED   },
        { TerminalType::TYPE_UNSIGNED, TypeSpec::UNSIGNED },
        { TerminalType::TYPE_FLOAT,    TypeSpec::FLOAT    },
        { TerminalType::TYPE_DOUBLE,   TypeSpec::DOUBLE   },
        { TerminalType::TYPE_STRING,   TypeSpec::STRING   }
    };

    for( auto p : type_map )
        if( parser.ExpectTerminal( p.first ) )
        {
            token.reset( new TypeSpecifier( p.second ) );
            return true;
        }

    return false;
}

//------------------------------------------------------------------------------
// TypeQualifier
//------------------------------------------------------------------------------

TypeQualifier::TypeQualifier( TypeQual t )
    :m_typeQual( t )
{
}

TypeQualifier::~TypeQualifier()
{
}

void TypeQualifier::Print( int depth ) const
{
}

bool TypeQualifier::Parse( Parser& parser,
                           std::unique_ptr<TypeQualifier>& token )
{
    const static std::vector<std::pair<TerminalType, TypeQual> > qual_map =
    {
        { TerminalType::CONST,     TypeQual::CONST    },
        { TerminalType::VOLATILE,  TypeQual::VOLATILE }
    };

    for( auto p : qual_map )
        if( parser.ExpectTerminal( p.first ) )
        {
            token.reset( new TypeQualifier( p.second ) );
            return true;
        }

    return false;
}

//------------------------------------------------------------------------------
// StorageClassSpecifier
//------------------------------------------------------------------------------

StorageClassSpecifier::StorageClassSpecifier( StorageClass storage_class )
    :m_storageClass( storage_class )
{
}

StorageClassSpecifier::~StorageClassSpecifier()
{
}

void StorageClassSpecifier::Print( int depth ) const
{
}

bool StorageClassSpecifier::Parse( Parser& parser,
                           std::unique_ptr<StorageClassSpecifier>& token )
{
    const static std::vector<std::pair<TerminalType, StorageClass> > sc_map =
    {
        { TerminalType::STATIC,     StorageClass::STATIC   },
        { TerminalType::EXTERN,     StorageClass::EXTERN   },
        { TerminalType::UNIFORM,    StorageClass::UNIFORM  },
        { TerminalType::VARYING,    StorageClass::VARYING  }
    };

    for( auto p : sc_map )
        if( parser.ExpectTerminal( p.first ) )
        {
            token.reset( new StorageClassSpecifier( p.second ) );
            return true;
        }

    return false;
}

} // namespace Compiler
} // namespace JoeLang
