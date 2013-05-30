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

#include <functional>
#include <memory>
#include <vector>

namespace JoeLang 
{

enum class Type;

namespace Compiler 
{

class Node;
using Node_up = std::unique_ptr<Node>;
using Node_ref = std::reference_wrapper<const Node>;
class TypeNode;
template<typename>
class ConstantNode;
class ZeroNode;
enum class NodeType;
class CompleteType;
class VariableNode;
class Variable;
using Variable_sp = std::shared_ptr<Variable>;
class FunctionNode;
class Function;
using Function_sp = std::shared_ptr<Function>;
class SwizzleNode;
class Swizzle;

class NodeManager
{
public:
    NodeManager();
    ~NodeManager();
    
    const Node& MakeNode( NodeType node_type, std::vector<Node_ref> children );
    
    const TypeNode& MakeTypeNode( const CompleteType& type );
    
    template<typename T>
    const ConstantNode<T>& MakeConstant( T constant_value );
    
    const ZeroNode& MakeZero( Type type );
    
    const VariableNode& MakeVariableNode( Variable_sp variable );
    
    const FunctionNode& MakeFunctionNode( Function_sp function );
    
    const SwizzleNode& MakeSwizzleNode(const Node& swizzled, const Swizzle& swizzle );
    
    // TODO make this a little better
    std::vector<Node_up> m_Nodes;
};

} // namespace Compiler
} // namespace JoeLang

#include "node_manager-inl.hpp"
