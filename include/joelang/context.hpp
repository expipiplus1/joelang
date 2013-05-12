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

#include <functional>
#include <memory>
#include <string>
#include <vector>

#include <joelang/config.h>

namespace JoeLang
{

class Effect;
class StateBase;

namespace Compiler
{
    class EffectFactory;
    class Runtime;
}

class Context
{
public:
    Context();
    ~Context();

    Effect* CreateEffectFromString( const std::string& source,
                                    const std::string& name );
    Effect* CreateEffectFromFile(   const std::string& filename );

#ifdef JOELANG_WITH_OPENGL
    /**
      * Registers a bunch of handy opengl states to be set during passes
      */
    void RegisterOpenGLStates();

    /**
      * Registers a bunch of handy opengl actions to be set during passes
      */
    void RegisterOpenGLActions();
#endif

    bool AddState( StateBase* state );
    const StateBase* GetNamedState( const std::string& name ) const;
    const std::vector<StateBase*>& GetStates() const;

    void SetErrorCallback( std::function<void(std::string)> error_callback );
    const std::function<void(std::string)>& GetErrorCallback() const;

    /**
      * Sends the error message to m_ErrorCallback
      * Used internally by JoeLang
      */
    void Error( const std::string& error_string ) const;
private:
    std::vector<StateBase*>                     m_States;
    std::vector<std::unique_ptr<Effect> >       m_Effects;

    std::function<void(std::string)>            m_ErrorCallback;

    std::unique_ptr<Compiler::Runtime>          m_Runtime;

    std::unique_ptr<Compiler::EffectFactory>    m_EffectFactory;
};

} // namespace JoeLang
