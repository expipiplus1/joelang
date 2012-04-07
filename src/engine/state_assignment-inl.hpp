/*
    Copyright 2012 Joe Hermaszewski. All rights reserved.

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

#pragma once

#include "state_assignment.hpp"

#include <functional>

#include <engine/state.hpp>

namespace JoeLang
{

template<typename T>
StateAssignment<T>::StateAssignment( const State<T>& state,
                                     std::function<T()> getter )
    :m_state(state)
    ,m_getter(std::move(getter))
{
}

template<typename T>
StateAssignment<T>::~StateAssignment()
{
}

template<typename T>
void StateAssignment<T>::SetState() const
{
    m_state.SetState( m_getter() );
}

template<typename T>
void StateAssignment<T>::ResetState() const
{
    m_state.ResetState();
}

template<typename T>
bool StateAssignment<T>::ValidateState() const
{
    return m_state.ValidateState();
}

template<typename T>
ConstStateAssignment<T>::ConstStateAssignment( const State<T>& state,
                                               T value )
    :m_state( state )
    ,m_value( std::move(value) )
{
}

template<typename T>
ConstStateAssignment<T>::~ConstStateAssignment()
{
}

template<typename T>
void ConstStateAssignment<T>::SetState() const
{
    m_state.SetState( m_value );
}

template<typename T>
void ConstStateAssignment<T>::ResetState() const
{
    m_state.ResetState();
}

template<typename T>
bool ConstStateAssignment<T>::ValidateState() const
{
    return m_state.ValidateState();
}

} // namespace JoeLang
