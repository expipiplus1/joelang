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

#include "node.hpp"

#include <cassert>
#include <vector>

namespace JoeLang
{
namespace Compiler
{

Node::Node( NodeType type, std::vector<Node_ref> children )
    : m_Type( type ),
      m_Children( std::move( children ) )
{
}

Node::~Node()
{
}

unsigned Node::GetNumChildren() const
{
    return m_Children.size();
}

const std::vector<Node_ref>& Node::GetChildren() const
{
    return m_Children;
}

const Node& Node::GetChild( unsigned index ) const
{
    assert( index < GetNumChildren() && "Trying to get an out of bounds child" );
    return m_Children[index];
}

std::set<const Node*> Node::GetDescendantsOfNodeType( NodeType node_type ) const
{
    std::set<const Node*> ret;
    std::vector<Node_ref> queue = { { *this } };

    while( !queue.empty() )
    {
        const Node& n = queue.back();
        if( n.GetNodeType() == node_type )
            ret.insert( &n );
        queue.pop_back();
        for( const Node& s : n.GetChildren() )
            queue.push_back( s );
    }

    return ret;
}

NodeType Node::GetNodeType() const
{
    return m_Type;
}

} // namespace Compiler
} // namespace JoeLang
