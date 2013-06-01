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

#pragma once

#include <compiler/code_dag/expression_node.hpp>
#include <compiler/code_dag/node.hpp>
#include <joelang/types.hpp>

namespace JoeLang
{
namespace Compiler
{

class CompleteType;
enum class NodeType;

class ConstantNodeBase : public ExpressionNode
{
public:
    Type GetType() const;

    /** Used for casting **/
    static
    bool classof( const Node* n );

protected:
    ConstantNodeBase( Type type );

private:
    Type m_Type;
};


template <typename T>
class ConstantNode : public ConstantNodeBase
{
public:
    using value_type = T;

    const value_type& GetConstant() const;

private:
    friend class NodeManager;
    ConstantNode( value_type constant_value );

    value_type m_Constant;
};

} // namespace Compiler
} // namespace JoeLang

#include "constant_node-inl.hpp"
