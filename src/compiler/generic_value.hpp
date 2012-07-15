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

#pragma once

#include <string>

#include <engine/types.hpp>

namespace JoeLang
{
namespace Compiler
{

class GenericValue
{
public:
    GenericValue();
    GenericValue( const GenericValue& g );
    explicit
    GenericValue( Type type );
    const GenericValue& operator = ( const GenericValue& g );

    explicit
    GenericValue( jl_bool   bool_value   );
    explicit
    GenericValue( jl_i8     i8_value     );
    explicit
    GenericValue( jl_i16    i16_value    );
    explicit
    GenericValue( jl_i32    i32_value    );
    explicit
    GenericValue( jl_i64    i64_value    );
    explicit
    GenericValue( jl_u8     u8_value     );
    explicit
    GenericValue( jl_u16    u16_value    );
    explicit
    GenericValue( jl_u32    u32_value    );
    explicit
    GenericValue( jl_u64    u64_value    );
    explicit
    GenericValue( jl_float  float_value  );
    explicit
    GenericValue( jl_double double_value );
    explicit
    GenericValue( jl_string string_value );

    ~GenericValue();

    /**
      * \returns The type of this genericvalue
      */
    Type GetType() const;

    jl_bool          GetBool() const;
    jl_i8            GetI8() const;
    jl_i16           GetI16() const;
    jl_i32           GetI32() const;
    jl_i64           GetI64() const;
    jl_u8            GetU8() const;
    jl_u16           GetU16() const;
    jl_u32           GetU32() const;
    jl_u64           GetU64() const;
    jl_float         GetFloat() const;
    jl_double        GetDouble() const;
    const jl_string& GetString() const;

    /**
      * \defgroup GenericValue Binary operators
      * \{
      */
    static
    GenericValue Lor ( const GenericValue& g1, const GenericValue& g2 );
    static
    GenericValue Land( const GenericValue& g1, const GenericValue& g2 );

    static
    GenericValue Or ( const GenericValue& g1, const GenericValue& g2 );
    static
    GenericValue Xor( const GenericValue& g1, const GenericValue& g2 );
    static
    GenericValue And( const GenericValue& g1, const GenericValue& g2 );

    static
    GenericValue EqualTo    ( const GenericValue& g1, const GenericValue& g2 );
    static
    GenericValue NotEqualTo ( const GenericValue& g1, const GenericValue& g2 );

    static
    GenericValue LessThan        ( const GenericValue& g1,
                                   const GenericValue& g2 );
    static
    GenericValue GreaterThan     ( const GenericValue& g1,
                                   const GenericValue& g2 );
    static
    GenericValue LessThanEqual   ( const GenericValue& g1,
                                   const GenericValue& g2 );
    static
    GenericValue GreaterThanEqual( const GenericValue& g1,
                                   const GenericValue& g2 );

    static
    GenericValue Shl    ( const GenericValue& g1, const GenericValue& g2 );
    static
    GenericValue Shr    ( const GenericValue& g1, const GenericValue& g2 );

    static
    GenericValue Add( const GenericValue& g1, const GenericValue& g2 );
    static
    GenericValue Sub( const GenericValue& g1, const GenericValue& g2 );
    static
    GenericValue Mul( const GenericValue& g1, const GenericValue& g2 );
    static
    GenericValue Div( const GenericValue& g1, const GenericValue& g2 );
    static
    GenericValue Mod( const GenericValue& g1, const GenericValue& g2 );
    /**
      * \}
      */

    /**
      * \defgroup GenericValue Unary operators
      * \{
      */
    static
    GenericValue UnaryPlus( const GenericValue& g );
    static
    GenericValue UnaryMinus( const GenericValue& g );
    static
    GenericValue LogicalNot( const GenericValue& g );
    static
    GenericValue BitwiseNot( const GenericValue& g );
    /**
      * \}
      */

    /**
      * \defgroup GenericValue Casting operators
      * \{
      */
    static
    GenericValue Cast( Type t, const GenericValue& g );
    /**
      * \}
      */

private:
    Type m_Type;

    union
    {
        jl_bool   m_BoolValue;
        jl_i8     m_I8Value;
        jl_i16    m_I16Value;
        jl_i32    m_I32Value;
        jl_i64    m_I64Value;
        jl_u8     m_U8Value;
        jl_u16    m_U16Value;
        jl_u32    m_U32Value;
        jl_u64    m_U64Value;
        jl_float  m_FloatValue;
        jl_double m_DoubleValue;
        jl_string m_StringValue;
    };
};

} // namespace Compiler
} // namespace JoeLang
