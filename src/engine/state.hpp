/*
    Copyright 2011 Joe Hermaszewski. All rights reserved.

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

#include <engine/types.hpp>

namespace JoeLang
{

void DefaultStateResetCallback();

bool DefaultStateValidateCallback();

class StateBase
{
public:
    explicit
    StateBase( std::string name );
    virtual
    ~StateBase();

    const std::string& GetName() const;

    virtual
    Type GetType() const = 0;

private:
    std::string m_Name;
};

template<typename T>
class State : public StateBase
{
    static_assert( JoeLangType<T>::value != Type::UNKNOWN_TYPE,
                   "Can't create a state with an unhandled type" );
public:
    State() = delete;
    State( std::string name, std::map<std::string, T> enumerations = {} );
    virtual
    ~State();

    void SetCallbacks( std::function<void(T)> set_callback,
                       std::function<void()>  reset_callback,
                       std::function<bool()>  validate_callback );

    //TODO enable passing by reference for large Ts
    void SetState( T value ) const;
    void ResetState() const;
    bool ValidateState() const;

    virtual
    Type GetType() const override;

    const std::map<std::string, T>& GetEnumerations() const;

private:
    std::map<std::string, T> m_Enumerations;

    std::function<void(T)> m_SetCallback;
    std::function<void()> m_ResetCallback;
    std::function<bool()> m_ValidateCallback;
};

} // namespace JoeLang

#include "state-inl.hpp"
