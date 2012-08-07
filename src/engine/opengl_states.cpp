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

#include "opengl_states.hpp"

#if defined(__APPLE__)
#include <OpenGL/gl.h>
#else
#include <GL/gl.h>
#endif

#include <joemath/joemath.hpp>

#include <joelang/context.hpp>
#include <joelang/state.hpp>

using namespace JoeMath;

namespace JoeLang
{

void RegisterOpenGLStates( Context& context )
{
    static
    State<float4> clear_color( "clear_color" );
    clear_color.SetCallbacks( [](float4 v)->void
                              {glClearColor(v.x(), v.y(), v.z(), v.w());},
                              []()->void
                              {glClearColor(0.f, 0.f, 0.f, 0.f);} );

    static
    State<float> clear_depth( "clear_depth" );
    clear_depth.SetCallbacks( [](float v)->void
                              {glClearDepth(v);},
                              []()->void
                              {glClearDepth(1.f);} );

    static
    State<s32> clear_stencil( "clear_stencil" );
    clear_stencil.SetCallbacks( [](s32 v)->void
                                {glClearStencil(v);},
                                []()->void
                                {glClearStencil(0);} );

    assert( context.AddState( &clear_color ) && "Error adding opengl state" );
    assert( context.AddState( &clear_depth ) && "Error adding opengl state" );
    assert( context.AddState( &clear_stencil ) && "Error adding opengl state" );
}

void RegisterOpenGLActions( Context& context )
{
    static
    State<u32> clear( "clear",
                      {{"COLOR",  GL_COLOR_BUFFER_BIT},
                       {"DEPTH",  GL_DEPTH_BUFFER_BIT},
                       {"ACCUM",  GL_ACCUM_BUFFER_BIT},
                       {"STENCIL",GL_STENCIL_BUFFER_BIT}} );
    clear.SetCallbacks( [](u32 v)->void
                        {glClear(v);} );

    assert( context.AddState( &clear ) && "Error adding opengl action" );
}

} // namespace JoeLang
