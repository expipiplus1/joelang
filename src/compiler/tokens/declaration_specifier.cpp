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

#include <algorithm>
#include <memory>

#include <compiler/casting.hpp>
#include <compiler/parser.hpp>
#include <compiler/sema_analyzer.hpp>
#include <compiler/terminal_types.hpp>
#include <compiler/tokens/token.hpp>
#include <compiler/type_properties.hpp>
#include <joelang/types.hpp>

namespace JoeLang
{
namespace Compiler
{

//------------------------------------------------------------------------------
// DeclSpecs
//------------------------------------------------------------------------------

DeclSpecs::DeclSpecs()
    :m_IsConst  ( false )
    ,m_IsUniform( false )
    ,m_IsVarying( false )
    ,m_IsStatic ( false )
    ,m_IsExtern ( false )
    ,m_IsIn     ( false )
    ,m_IsOut    ( false )
    ,m_IsInline ( false )
    ,m_Type     ( Type::UNKNOWN )
{
}

bool DeclSpecs::AnalyzeDeclSpecs(
                         const std::vector<DeclarationSpecifier_up>& decl_specs,
                         SemaAnalyzer& sema )
{
    bool good = true;
    std::vector<TypeSpec> type_specs;

    for( const auto& t : decl_specs )
    {
        if( isa<TypeQualifierSpecifier>(t) )
        {
            TypeQualifierSpecifier& type_qual =
                               static_cast<TypeQualifierSpecifier&>( *t.get() );
            switch( type_qual.GetQualifier() )
            {
            case TypeQualifier::CONST:
                m_IsConst = true;
                break;
            case TypeQualifier::VOLATILE:
                sema.Warning( "volatile specfier ignored in declaration" );
                break;
            case TypeQualifier::INLINE:
                m_IsInline = true;
                break;
            }
        }
        else if( isa<TypeSpecifier>(t) )
        {
            TypeSpecifier& type_spec = static_cast<TypeSpecifier&>( *t.get() );
            type_specs.push_back( type_spec.GetSpecifier() );
        }
        else if( isa<StorageClassSpecifier>(t) )
        {
            StorageClassSpecifier& storage_class =
                                static_cast<StorageClassSpecifier&>( *t.get() );
            switch( storage_class.GetStorageClass() )
            {
            case StorageClass::UNIFORM:
                m_IsUniform = true;
                break;
            case StorageClass::VARYING:
                m_IsVarying = true;
                break;
            case StorageClass::STATIC:
                m_IsStatic = true;
                break;
            case StorageClass::EXTERN:
                m_IsExtern = true;
                break;
            case StorageClass::IN:
                m_IsIn = true;
                break;
            case StorageClass::OUT:
                m_IsOut = true;
                break;
            case StorageClass::INOUT:
                m_IsIn = true;
                m_IsOut = true;
                break;
            }
        }
    }

    if( m_IsUniform && m_IsVarying )
    {
        sema.Error( "Can't create a variable that's both uniform and varying" );
        good = false;
    }

    if( m_IsStatic && m_IsUniform )
    {
        sema.Error( "Can't combine 'static' and 'uniform' specifiers" );
        good = false;
    }

    if( m_IsStatic && m_IsVarying )
    {
        sema.Error( "Can't combine 'static' and 'varying' specifiers" );
        good = false;
    }

    if( m_IsStatic && (m_IsIn || m_IsOut) )
    {
        sema.Error( "Can't have 'in', 'out' or 'inout' specifiers on a static "
                    "variable" );
        good = false;
    }

    if( m_IsVarying )
    {
        //
        // If this is varying it must be declared with in, out or inout
        //
        if( !(m_IsIn || m_IsOut) )
        {
            sema.Error( "Varyings must have an 'in', 'out' or 'inout' "
                        "specifier" );
            good = false;
        }
    }

    if( m_IsUniform )
    {
        if( m_IsIn || m_IsOut )
        {
            sema.Error( "Uniforms can't have an 'in', 'out' or 'inout' "
                        "specifiers" );
            good = false;
        }
    }

    //
    // Get the type from the specifiers
    //
    m_Type = DeduceType( std::move(type_specs), sema );

    if( m_Type == Type::UNKNOWN )
    {
        sema.Error( "No type in declaration specifier" );
        good = false;
    }

    return good;
}

void DeclSpecs::SetIsIn( bool is_in )
{
    m_IsIn = is_in;
}

void DeclSpecs::SetIsUniform( bool is_uniform )
{
    m_IsUniform = is_uniform;
}

void DeclSpecs::SetIsVarying( bool is_varying )
{
    m_IsVarying = is_varying;
}

/// TODO rename to has const specifier
bool DeclSpecs::IsConst() const
{
    return m_IsConst;
}

bool DeclSpecs::IsUniform() const
{
    return m_IsUniform;
}

bool DeclSpecs::IsVarying() const
{
    return m_IsVarying;
}

bool DeclSpecs::IsStatic() const
{
    return m_IsStatic;
}

bool DeclSpecs::IsIn() const
{
    return m_IsIn;
}

bool DeclSpecs::IsOut() const
{
    return m_IsOut;
}

bool DeclSpecs::IsInline() const
{
    return m_IsInline;
}

Type DeclSpecs::GetType() const
{
    return m_Type;
}

Type DeclSpecs::DeduceType( std::vector<TypeSpec> type_specs,
                            SemaAnalyzer& sema )
{
    bool is_signed = false;
    bool has_type = false;
    Type type = Type::UNKNOWN;

    // Sort the type specifiers to ease combination
    std::sort( type_specs.begin(), type_specs.end() );

    /// TODO do this in a better way
    for( auto ts : type_specs )
    {
        switch( ts )
        {
        case TypeSpec::VOID:
            if( has_type )
                sema.Error( "Can't combine void with other type " +
                            GetTypeString( type ) );
            type = Type::VOID;
            has_type = true;
            break;
        case TypeSpec::STRING:
            if( has_type )
                sema.Error( "Can't combine string with other type " +
                            GetTypeString( type ) );
            type = Type::STRING;
            has_type = true;
            break;
        case TypeSpec::FLOAT:
            if( has_type )
                sema.Error( "Can't combine float with other type " +
                            GetTypeString( type ) );
            type = Type::FLOAT;
            has_type = true;
            break;
        case TypeSpec::FLOAT2:
            if( has_type )
                sema.Error( "Can't combine float2 with other type " +
                            GetTypeString( type ) );
            type = Type::FLOAT2;
            has_type = true;
            break;
        case TypeSpec::FLOAT3:
            if( has_type )
                sema.Error( "Can't combine float3 with other type " +
                            GetTypeString( type ) );
            type = Type::FLOAT3;
            has_type = true;
            break;
        case TypeSpec::FLOAT4:
            if( has_type )
                sema.Error( "Can't combine float4 with other type " +
                            GetTypeString( type ) );
            type = Type::FLOAT4;
            has_type = true;
            break;
        case TypeSpec::DOUBLE:
            if( has_type )
                sema.Error( "Can't combine double with other type " +
                            GetTypeString( type ) );
            type = Type::DOUBLE;
            has_type = true;
            break;
        case TypeSpec::BOOL:
            if( has_type )
                sema.Error( "Can't combine bool with other type " +
                            GetTypeString( type ) );
            type = Type::BOOL;
            has_type = true;
            break;
        case TypeSpec::CHAR:
            if( has_type )
                sema.Error( "Can't combine char with other type " +
                            GetTypeString( type ) );
            // Types are signed by default
            type = Type::CHAR;
            has_type = true;
            break;
        case TypeSpec::SHORT:
            if( has_type )
                sema.Error( "Can't combine short with other type " +
                            GetTypeString( type ) );
            // Types are signed by default
            type = Type::SHORT;
            has_type = true;
            break;
        case TypeSpec::INT:
            if( has_type )
            {
                if( !IsIntegral( type ) ||
                    type == Type::BOOL ||
                    type == Type::INT )
                    sema.Error( "Can't combine int with other type " +
                            GetTypeString( type ) );
                // Don't change the type
            }
            else
            {
                type = Type::INT;
                has_type = true;
            }
            break;
        case TypeSpec::LONG:
            if( has_type )
            {
                if( !IsIntegral( type ) ||
                    type == Type::BOOL ||
                    type == Type::CHAR ||
                    type == Type::SHORT )
                    sema.Error( "Can't combine long with other type " +
                                GetTypeString( type ) );
            }
            has_type = true;
            type = Type::LONG;
            break;
        case TypeSpec::SIGNED:
            if( has_type )
            {
                if( !IsIntegral( type ) ||
                    type == Type::BOOL )
                    sema.Error( "Can't combine signed with other type " +
                                GetTypeString( type ) );
                // Types are signed by default, no need to make them signed
            }
            else
            {
                has_type = true;
                type = Type::INT;
            }
            is_signed = true;
            break;
        case TypeSpec::UNSIGNED:
            if( is_signed )
                sema.Error( "Declaration can't be signed and unsigned" );
            if( has_type )
            {
                if( !IsIntegral( type ) ||
                    type == Type::BOOL )
                    sema.Error( "Can't combine unsigned with other type: " +
                                GetTypeString( type ) );
                else
                    type = MakeUnsigned( type );
            }
            else
            {
                has_type = true;
                type = Type::UINT;
            }
            break;
        }
    }

    return type;
}

//------------------------------------------------------------------------------
// DeclarationSpecifier
//------------------------------------------------------------------------------

DeclarationSpecifier::DeclarationSpecifier( TokenTy sub_class_id )
    :Token( sub_class_id )
{
}

DeclarationSpecifier::~DeclarationSpecifier()
{
}

bool DeclarationSpecifier::Parse( Parser& parser,
                                  std::unique_ptr<DeclarationSpecifier>& token )
{
    std::unique_ptr<Token> t;
    if( !parser.ExpectAnyOf<TypeSpecifier,
                            TypeQualifierSpecifier,
                            StorageClassSpecifier>( t ) )
        return false;

    token.reset( static_cast<DeclarationSpecifier*>( t.release() ) );
    return true;
}

bool DeclarationSpecifier::classof( const Token* d )
{
    return d->GetSubClassID() >= TokenTy::DeclarationSpecifier_Start &&
           d->GetSubClassID() <= TokenTy::DeclarationSpecifier_End;
}

bool DeclarationSpecifier::classof( const DeclarationSpecifier* d )
{
    // A DeclarationSpecifier is always a DeclarationSpecifier
    return true;
}

//------------------------------------------------------------------------------
// TypeSpecifier
//------------------------------------------------------------------------------

TypeSpecifier::TypeSpecifier( TypeSpec t )
    :DeclarationSpecifier( TokenTy::TypeSpecifier )
    ,m_TypeSpec( t )
{
}

TypeSpecifier::~TypeSpecifier()
{
}

TypeSpec TypeSpecifier::GetSpecifier() const
{
    return m_TypeSpec;
}

Type TypeSpecifier::GetType() const
{
    const static std::map<TypeSpec, Type> type_map =
    {
        { TypeSpec::VOID,     Type::VOID   },
        { TypeSpec::BOOL,     Type::BOOL   },
        { TypeSpec::CHAR,     Type::CHAR     },
        { TypeSpec::SHORT,    Type::SHORT    },
        { TypeSpec::INT,      Type::INT    },
        { TypeSpec::LONG,     Type::LONG    },
        { TypeSpec::SIGNED,   Type::INT    },
        { TypeSpec::UNSIGNED, Type::UINT    },
        { TypeSpec::FLOAT,    Type::FLOAT  },
        { TypeSpec::FLOAT2,   Type::FLOAT2 },
        { TypeSpec::FLOAT3,   Type::FLOAT3 },
        { TypeSpec::FLOAT4,   Type::FLOAT4 },
        { TypeSpec::DOUBLE,   Type::DOUBLE },
        { TypeSpec::STRING,   Type::STRING }
    };

    return type_map.at( m_TypeSpec );
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
        { TerminalType::TYPE_FLOAT2,   TypeSpec::FLOAT2   },
        { TerminalType::TYPE_FLOAT3,   TypeSpec::FLOAT3   },
        { TerminalType::TYPE_FLOAT4,   TypeSpec::FLOAT4   },
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

bool TypeSpecifier::classof( const DeclarationSpecifier* d )
{
    return d->GetSubClassID() == TokenTy::TypeSpecifier;
}

bool TypeSpecifier::classof( const TypeSpecifier* d )
{
    return true;
}

//------------------------------------------------------------------------------
// TypeQualifierSpecifier
//------------------------------------------------------------------------------

TypeQualifierSpecifier::TypeQualifierSpecifier( TypeQualifier t )
    :DeclarationSpecifier( TokenTy::TypeQualifierSpecifier )
    ,m_TypeQualifier( t )
{
}

TypeQualifierSpecifier::~TypeQualifierSpecifier()
{
}

TypeQualifier TypeQualifierSpecifier::GetQualifier() const
{
    return m_TypeQualifier;
}

bool TypeQualifierSpecifier::Parse( Parser& parser,
                                    TypeQualifierSpecifier_up& token )
{
    const static std::vector<std::pair<TerminalType, TypeQualifier> > qual_map =
    {
        { TerminalType::CONST,     TypeQualifier::CONST    },
        { TerminalType::VOLATILE,  TypeQualifier::VOLATILE },
        { TerminalType::INLINE,    TypeQualifier::INLINE   }
    };

    for( auto p : qual_map )
        if( parser.ExpectTerminal( p.first ) )
        {
            token.reset( new TypeQualifierSpecifier( p.second ) );
            return true;
        }

    return false;
}

bool TypeQualifierSpecifier::classof( const DeclarationSpecifier* d )
{
    return d->GetSubClassID() == TokenTy::TypeQualifierSpecifier;
}

bool TypeQualifierSpecifier::classof( const TypeQualifierSpecifier* d )
{
    return true;
}

//------------------------------------------------------------------------------
// StorageClassSpecifier
//------------------------------------------------------------------------------

StorageClassSpecifier::StorageClassSpecifier( StorageClass storage_class )
    :DeclarationSpecifier( TokenTy::StorageClassSpecifier )
    ,m_StorageClass( storage_class )
{
}

StorageClassSpecifier::~StorageClassSpecifier()
{
}

StorageClass StorageClassSpecifier::GetStorageClass() const
{
    return m_StorageClass;
}

bool StorageClassSpecifier::Parse( Parser& parser,
                           std::unique_ptr<StorageClassSpecifier>& token )
{
    const static std::vector<std::pair<TerminalType, StorageClass> > sc_map =
    {
        { TerminalType::STATIC,  StorageClass::STATIC   },
        { TerminalType::EXTERN,  StorageClass::EXTERN   },
        { TerminalType::UNIFORM, StorageClass::UNIFORM  },
        { TerminalType::VARYING, StorageClass::VARYING  },
        { TerminalType::IN,      StorageClass::IN       },
        { TerminalType::OUT,     StorageClass::OUT      },
        { TerminalType::INOUT,   StorageClass::INOUT    }
    };

    for( auto p : sc_map )
        if( parser.ExpectTerminal( p.first ) )
        {
            token.reset( new StorageClassSpecifier( p.second ) );
            return true;
        }

    return false;
}

bool StorageClassSpecifier::classof( const DeclarationSpecifier* d )
{
    return d->GetSubClassID() == TokenTy::StorageClassSpecifier;
}

bool StorageClassSpecifier::classof( const StorageClassSpecifier* d )
{
    return true;
}

} // namespace Compiler
} // namespace JoeLang
