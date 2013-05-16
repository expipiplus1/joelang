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

#include <string>

#include <joelang/types.hpp>

namespace JoeLang
{

class ParameterBase
{
public:
    explicit
    ParameterBase( std::string name );
    virtual
    ~ParameterBase();

    const std::string& GetName() const;

    virtual
    Type GetType() const = 0;

private:
    std::string m_Name;
};

template<typename T>
class Parameter : public ParameterBase
{
    static_assert( JoeLangType<T>::value != Type::UNKNOWN,
                   "Can't create a Parameter with an unhandled type" );
public:
    Parameter( std::string name, T& data_location );
    virtual
    ~Parameter() = default;

    void SetParameter( T value );
    T    GetParameter() const;

    virtual
    Type GetType() const override;

private:
    //
    // This typically points to the data for this variable in the LLVM execution
    // engine, but there's no reason it couldn't point to somewhere else
    //
    T&  m_Data;
};

} // namespace JoeLang

#include "inl/parameter-inl.hpp"
