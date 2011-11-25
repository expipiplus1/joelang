/*
    Copyright 2011 Joe Hermaszewski. All rights reserved.

    Redistribution and use in source and binary forms, with or without modification, are
    permitted provided that the following conditions are met:

    1. Redistributions of source code must retain the above copyright notice, this list of
    conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright notice, this list
    of conditions and the following disclaimer in the documentation and/or other materials
    provided with the distribution.

    THIS SOFTWARE IS PROVIDED BY JOE HERMASZEWSKI "AS IS" AND ANY EXPRESS OR IMPLIED
    WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
    FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL JOE HERMASZEWSKI OR
    CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
    CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
    SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
    ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
    NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
    ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

    The views and conclusions contained in the software and documentation are those of the
    authors and should not be interpreted as representing official policies, either expressed
    or implied, of Joe Hermaszewski.
*/

#include "driver.hpp"

#include <fstream>
#include <iostream>
#include <string>
#include <sstream>
#include "scanner.hpp"

namespace JoeLang
{
    Driver::Driver()
        : m_traceScanning( false )
        , m_traceParsing( false )
    {
    }

    Scanner* Driver::GetLexer() const
    {
        return m_lexer;
    }

    void Driver::SetLexer( Scanner* lexer )
    {
        m_lexer = lexer;
    }

    std::string& Driver::GetStreamName()
    {
        return m_streamName;
    }

    bool Driver::parse_stream( std::istream &in, const std::string &sname )
    {
        m_streamName = sname;

        Scanner scanner( &in );
        scanner.set_debug( m_traceScanning );
        m_lexer = &scanner;

        Parser parser( *this );
        parser.set_debug_level( m_traceParsing );
        return parser.parse() == 0;
    }

    bool Driver::parse_string(const std::string &input, const std::string &sname)
    {
        std::istringstream string_stream( input );
        return parse_stream( string_stream, sname );
    }

    bool Driver::parse_file(const std::string &filename)
    {
        std::ifstream file_stream( filename );
        if( file_stream.good() )
            return parse_stream( file_stream, filename );
        else
            return false;
    }

    void Driver::error(const location &l, const std::string &m)
    {
        std::cerr << "ERROR: " << l << ": " << m << std::endl;
    }

    void Driver::error(const std::string &m)
    {
        std::cerr << "ERROR: " << m << std::endl;
    }

} // namespace JoeLang
