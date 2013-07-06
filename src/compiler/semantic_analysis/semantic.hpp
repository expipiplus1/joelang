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

#include <memory>
#include <string>

namespace JoeLang
{

enum class ShaderDomain;

namespace Compiler
{

enum class SemanticType
{
    NO_SEMANTIC,

    CUSTOM,

    POSITION,
    VERTEXID,
    WPOS,
    DEPTH,
    COLOR,
};

class Semantic
{
public:
    /** This initializes semantic to NO_SEMANTIC type **/
    Semantic  ();
    /** This constructor asserts on an empty string **/
    explicit
    Semantic  ( std::string string );
    /** This constructor asserts on an empty string **/
    Semantic  ( std::string string, unsigned index );

    /**
     * \returns true if this represents NO_SEMANTIC
     */
    bool IsVoid() const;
    
    /**
      * Returns true if this semantic represents a varying
      */
    bool IsVarying() const;
    
    /**
      * Returns true if this semantic has a builtin for a particular domain
      */
    bool HasBuiltin( ShaderDomain domain, bool input ) const;
    
    /**
      * Returns the builtin string
      */
    const std::string& GetBuiltin( ShaderDomain domain, bool input ) const;

    bool HasIndex() const;
    /** This asserts that we have an index **/
    unsigned GetIndex() const;

private:
    void DetermineType();

    SemanticType  m_Type;
    std::string   m_String;
    bool          m_HasIndex;
    unsigned      m_Index;
};


} // namespace Compiler
} // namespace JoeLang
