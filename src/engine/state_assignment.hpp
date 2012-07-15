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

#include <engine/types.hpp>

namespace JoeLang
{

template<typename T>
class State;

class StateAssignmentBase
{
public:
    virtual
    ~StateAssignmentBase();

    virtual
    void SetState() const = 0;

    virtual
    void ResetState() const = 0;

    virtual
    bool ValidateState() const = 0;
};

template<typename T>
class StateAssignment : public StateAssignmentBase
{
    static_assert( JoeLangType<T>::value != Type::UNKNOWN_TYPE,
                   "Can't create a StateAssignment with an unhandled type" );
public:
    StateAssignment( const State<T>& state, std::function<T()> getter );
    virtual
    ~StateAssignment();

    virtual
    void SetState() const override;

    virtual
    void ResetState() const override;

    virtual
    bool ValidateState() const override;

private:
    const State<T>& m_State;
    std::function<T()> m_Getter;
};

template<typename T>
class ConstStateAssignment : public StateAssignmentBase
{
    static_assert( JoeLangType<T>::value != Type::UNKNOWN_TYPE,
                   "Can't create a ConstStateAssignment with unhandled type" );
public:
    ConstStateAssignment( const State<T>& state, T value );
    virtual
    ~ConstStateAssignment();

    virtual
    void SetState() const override;

    virtual
    void ResetState() const override;

    virtual
    bool ValidateState() const override;

private:
    const State<T>& m_State;
    T m_Value;
};

} // namespace JoeLang

#include "state_assignment-inl.hpp"
