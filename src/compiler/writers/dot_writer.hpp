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
#include <map>
#include <string>
#include <vector>

namespace JoeLang
{
namespace Compiler
{

class Node;
using Node_ref = std::reference_wrapper<const Node>;

///
/// \brief A class to dump the code dag to a dot file
///
class DotWriter
{
public:
    std::string GenerateDotString();
    void AddCluster( const Node& node, std::string name );
    void Clear();

private:
    bool HasSeen( const Node& node ) const;
    std::string GetEdges( const Node& node );
    std::string GetLabels() const;
    std::string GetIdentifier( const Node& node );
    std::string GetUniqueIdentifier();
    std::string GetNodeDescription( const Node& node ) const;

    struct NodeCluster
    {
        const Node& node;
        const std::string name;
    };

    std::map<const Node*, std::string> m_Identifiers;
    std::vector<NodeCluster> m_Clusters;
    unsigned m_NumUniqueIdentifiers = 0;
};

} // namespace Compiler
} // namespace JoeLang
