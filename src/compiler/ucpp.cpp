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

#include "ucpp.hpp"

#include <cassert>
#include <cstdio>

#include <map>
#include <memory>
#include <mutex>
#include <string>

#include <compiler/terminal_types.hpp>

#include <ucpp/cpp.h>

namespace JoeLang
{
namespace Compiler
{

const long UCPPLexerState::s_LexerFlags = WARN_STANDARD |
                                          WARN_ANNOYING |
                                          WARN_TRIGRAPHS |
                                          WARN_TRIGRAPHS_MORE |
                                          WARN_PRAGMA |
                                          CPLUSPLUS_COMMENTS |
                                          HANDLE_ASSERTIONS |
                                          MACRO_VAARG |
                                          LEXER |
                                          HANDLE_TRIGRAPHS;

std::mutex g_UCPPMutex;
std::unique_ptr<UCPPContext> g_UCPPContext;

void InitializeUCPP()
{
    static std::once_flag once_flag;
    std::call_once( once_flag, [](){g_UCPPContext.reset(new UCPPContext);} );
}

UCPPContext::UCPPContext()
{
    // Lock the ucpp resources
    std::unique_lock<std::mutex> lock(g_UCPPMutex);

    // Initialize static data
    init_cpp();

    // Define special macros such as __FILE__
    no_special_macros = 0;

    // Don't define __STDC_VERSION__
    c99_compliant = 0;

    // Don't define __STDC_HOSTED__
    c99_hosted = -1;

    // Initialize the macro table including assertions
    init_tables(true);

    // Set no include paths
    init_include_path(nullptr);

    // Emit all dependencies
    emit_dependencies = false;
    // emit #define macros
    emit_defines = false;
    // emit assertions
    emit_assertions = false;
    // don't emit things to a file
    emit_output = nullptr;
    // no transient characters
    transient_characters = nullptr;
}

UCPPContext::~UCPPContext()
{
    /*
    char filename[] = "testfile";
    set_init_filename(filename, false);

    int error;
    while(!(error = lex(&ls)))
    {
        std::cout << "File: " << current_filename <<
                     " Line: " << ls.line <<
                     " type: " << ls.ctok->type <<
                     " tok: " << ls.ctok->name << "\n";
    }
    if( error != CPPERR_EOF )
    {
        std::cout << "Error lexing: " << error << std::endl;
    }
    std::fclose(f);
    */

    // Lock the ucpp resources
    std::unique_lock<std::mutex> lock(g_UCPPMutex);
    wipeout();
}

UCPPLexerState::UCPPLexerState( const std::string& filename, FILE* file )
{
    //
    // This file shouldn't be opened by ucpp
    //
    set_init_filename( filename.c_str(), false );

    //
    // Initialize the lexer members
    //
    init_lexer_state( &m_LexerState );

    //
    // Set the lexer to be a lexer
    //
    init_lexer_mode( &m_LexerState );
    
    //
    // Load the file
    //
    m_LexerState.input = file;

    m_LexerState.flags = s_LexerFlags;
}

UCPPLexerState::~UCPPLexerState()
{
    m_LexerState.input = nullptr;
    free_lexer_state( &m_LexerState );
}

TerminalType UCPPLexerState::Lex( std::string& string )
{
    const static std::map<int, TerminalType> s_TokenMap =
    {
        { NONE,     TerminalType::WHITESPACE            }, // whitespace 
        { OPT_NONE, TerminalType::WHITESPACE            },
        { NEWLINE,  TerminalType::WHITESPACE            },
        { COMMENT,  TerminalType::COMMENT               }, // comment 
        { NUMBER,   TerminalType::INTEGER_LITERAL       }, // number constant 
        //{ NAME,   TerminalType:: // identifier 
        { BUNCH,    TerminalType::UNKNOWN_CHARACTER     }, // non-C characters 
        { PRAGMA,   TerminalType::UNKNOWN_CHARACTER     }, // a #pragma directive 
        { CONTEXT,  TerminalType::UNKNOWN_CHARACTER     }, // new file or #line 
        { STRING,   TerminalType::STRING_LITERAL        }, // constant "xxx" 
        { CHAR,     TerminalType::CHARACTER_LITERAL     }, // constant 'xxx' 
        { SLASH,    TerminalType::DIVIDE                }, // / 
        { ASSLASH,  TerminalType::DIVIDE_EQUALS         }, // /= 
        { MINUS,    TerminalType::MINUS                 }, // - 
        { MMINUS,   TerminalType::DECREMENT             }, // -- 
        { ASMINUS,  TerminalType::MINUS_EQUALS          }, // -= 
        { ARROW,    TerminalType::ARROW                 }, // -> 
        { PLUS,     TerminalType::PLUS                  }, // + 
        { PPLUS,    TerminalType::INCREMENT             }, // ++ 
        { ASPLUS,   TerminalType::PLUS_EQUALS           }, // += 
        { LT,       TerminalType::LESS_THAN             }, // < 
        { LEQ,      TerminalType::LESS_THAN_EQUALS      }, // <= 
        { LSH,      TerminalType::LEFT_SHIFT            }, // << 
        { ASLSH,    TerminalType::LEFT_SHIFT_EQUALS     }, // <<= 
        { GT,       TerminalType::GREATER_THAN          }, // > 
        { GEQ,      TerminalType::GREATER_THAN_EQUALS   }, // >= 
        { RSH,      TerminalType::RIGHT_SHIFT           }, // >> 
        { ASRSH,    TerminalType::RIGHT_SHIFT_EQUALS    }, // >>= 
        { ASGN,     TerminalType::EQUALS                }, // = 
        { SAME,     TerminalType::EQUALITY              }, // == 
        { NOT,      TerminalType::BITWISE_NOT           }, // ~ 
        { NEQ,      TerminalType::NOT_EQUALITY          }, // != 
        { AND,      TerminalType::AND                   }, // & 
        { LAND,     TerminalType::LOGICAL_AND           }, // && 
        { ASAND,    TerminalType::AND_EQUALS            }, // &= 
        { OR,       TerminalType::INCLUSIVE_OR          }, // | 
        { LOR,      TerminalType::LOGICAL_OR            }, // || 
        { ASOR,     TerminalType::INCLUSIVE_OR_EQUALS   }, // |= 
        { PCT,      TerminalType::MODULO                }, // % 
        { ASPCT,    TerminalType::MODULO_EQUALS         }, // %= 
        { STAR,     TerminalType::MULTIPLY              }, // * 
        { ASSTAR,   TerminalType::MULTIPLY_EQUALS       }, // *= 
        { CIRC,     TerminalType::EXCLUSIVE_OR          }, // ^ 
        { ASCIRC,   TerminalType::EXCLUSIVE_OR_EQUALS   }, // ^= 
        { LNOT,     TerminalType::LOGICAL_NOT           }, // ! 
        { LBRA,     TerminalType::OPEN_BRACE            }, // { 
        { RBRA,     TerminalType::CLOSE_BRACE           }, // } 
        { LBRK,     TerminalType::OPEN_SQUARE           }, // [ 
        { RBRK,     TerminalType::CLOSE_SQUARE          }, // ] 
        { LPAR,     TerminalType::OPEN_ROUND            }, // ( 
        { RPAR,     TerminalType::CLOSE_ROUND           }, // ) 
        { COMMA,    TerminalType::COMMA                 }, // , 
        { QUEST,    TerminalType::QUERY                 }, // ? 
        { SEMIC,    TerminalType::SEMICOLON             }, // ; 
        { COLON,    TerminalType::COLON                 }, // : 
        { DOT,      TerminalType::PERIOD                }, // . 
        { MDOTS,    TerminalType::UNKNOWN_CHARACTER     }, // ... 
        { SHARP,    TerminalType::UNKNOWN_CHARACTER     }, // # 
        { DSHARP,   TerminalType::UNKNOWN_CHARACTER     }, // ## 
        { UPLUS,    TerminalType::PLUS                  }, // unary + 
        { UMINUS,   TerminalType::MINUS                 }  // unary - 
    };
    
    const static std::map<std::string, TerminalType> s_KeywordMap =
    {
        { "technique",      TerminalType::TECHNIQUE,     },
        { "pass",           TerminalType::PASS,          },
        { "compile",        TerminalType::COMPILE,       },
        { "pixel_shader",   TerminalType::PIXEL_SHADER,  },
        { "vertex_shader",  TerminalType::VERTEX_SHADER, },
        { "return",         TerminalType::RETURN,        },
     
        //
        // Storage class specifiers
        //
        { "static",         TerminalType::STATIC,        },
        { "extern",         TerminalType::EXTERN,        },
        { "uniform",        TerminalType::UNIFORM,       },
        { "varying",        TerminalType::VARYING,       },
        { "in",             TerminalType::IN,            },
        { "out",            TerminalType::OUT,           },
        { "inout",          TerminalType::INOUT,         },
     
        //
        // Type qualifiers
        //
        { "const",          TerminalType::CONST,         },
        { "volatile",       TerminalType::VOLATILE,      },
        { "inline",         TerminalType::INLINE,        },
     
        //
        // Types
        //
        { "void",           TerminalType::TYPE_VOID,     },
        { "bool",           TerminalType::TYPE_BOOL,     },
        { "char",           TerminalType::TYPE_CHAR,     },
        { "short",          TerminalType::TYPE_SHORT,    },
        { "int",            TerminalType::TYPE_INT,      },
        { "long",           TerminalType::TYPE_LONG,     },
        { "float",          TerminalType::TYPE_FLOAT,    },
        { "float2",         TerminalType::TYPE_FLOAT2,   },
        { "float3",         TerminalType::TYPE_FLOAT3,   },
        { "float4",         TerminalType::TYPE_FLOAT4,   },
        { "double",         TerminalType::TYPE_DOUBLE,   },
        { "signed",         TerminalType::TYPE_SIGNED,   },
        { "unsigned",       TerminalType::TYPE_UNSIGNED, },
        { "string",         TerminalType::TYPE_STRING,   },
     
        //
        // Constants
        //
        { "true",           TerminalType::TRUE,          },
        { "false",          TerminalType::FALSE,         }
    };
    
    int lex_result = lex( &m_LexerState );
    if( lex_result == CPPERR_EOF )
        return TerminalType::END_OF_INPUT;
    assert( lex_result == 0 && "Error lexing" );
    
    string = m_LexerState.ctok->name;
    int ucpp_token = m_LexerState.ctok->type;
    
    //
    // If we have an identifier check to see if it's a keyword
    //
    if( ucpp_token == NAME )
    {
        auto k = s_KeywordMap.find( string );
        if( k != s_KeywordMap.end() )
            return k->second;
        return TerminalType::IDENTIFIER;
    }
    
    //
    // If we have a number we need to know if it's floating point
    //
    if( ucpp_token == NUMBER )
    {
        if( ReadFloatingLiteral( string.begin(), string.end() ) == 
            string.end() - string.begin() )
            return TerminalType::FLOATING_LITERAL;
        return TerminalType::INTEGER_LITERAL;
    }
    
    assert( s_TokenMap.find( ucpp_token ) != s_TokenMap.end() &&
            "Unknown token from ucpp" );
    return s_TokenMap.at( ucpp_token );
}

unsigned UCPPLexerState::GetLineNumber()
{
    return m_LexerState.line;
}

} // namespace Compiler
} // namespace JoeLang
