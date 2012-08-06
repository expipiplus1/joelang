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

#include <joelang/state.hpp>

#include <functional>
#include <map>
#include <string>
#include <utility>
#include <vector>

#include <joelang/types.hpp>

namespace JoeLang
{

template<typename T>
void DefaultStateSetCallback( T value )
{
}

template<typename T>
State<T>::State( std::string name, std::map< std::string, T > enumerations )
    :StateBase( std::move(name) )
    ,m_Enumerations( std::move(enumerations) )
    ,m_SetCallback( DefaultStateSetCallback<T> )
    ,m_ResetCallback( DefaultStateResetCallback )
    ,m_ValidateCallback( DefaultStateValidateCallback )
{
}

template<typename T>
State<T>::~State()
{
}

template<typename T>
void State<T>::SetCallbacks( std::function<void(T)> set_callback,
                             std::function<void()> reset_callback,
                             std::function<bool()> validate_callback )
{
    m_SetCallback   = set_callback ? set_callback
                                   : DefaultStateSetCallback<T>;
    m_ResetCallback = reset_callback ? reset_callback
                                     : DefaultStateResetCallback;
    m_ValidateCallback   = validate_callback ? validate_callback
                                             : DefaultStateValidateCallback;
}

template<typename T>
void State<T>::SetState( T value ) const
{
    m_SetCallback( value );
}

template<typename T>
void State<T>::ResetState() const
{
    m_ResetCallback();
}

template<typename T>
bool State<T>::ValidateState() const
{
    return m_ValidateCallback();
}

template<typename T>
std::vector<std::string> State<T>::GetEnumerantNames() const
{
    std::vector<std::string> ret;
    ret.reserve( m_Enumerations.size() );
    for( const auto& e : m_Enumerations )
        ret.push_back( e.first );
    return std::move(ret);
}

template<typename T>
Type State<T>::GetType() const
{
    return JoeLangType<T>::value;
}

template<typename T>
const std::map<std::string, T>& State<T>::GetEnumerations() const
{
    return m_Enumerations;
}

} // namespace JoeLang
