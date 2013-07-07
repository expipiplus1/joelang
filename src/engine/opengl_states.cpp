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

#include <joelang/config.h>

#ifdef JOELANG_WITH_OPENGL

//#include <GL/GLee.h>

#if defined(__APPLE__)
#include <OpenGL/gl3.h>
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
    //
    // Clearing
    //

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

    //
    // Blending
    //

    static
    State<float4> blend_color( "blend_color" );
    blend_color.SetCallbacks( [](float4 v)->void
                              {glBlendColor(v.x(), v.y(), v.z(), v.w());},
                              []()->void
                              {glBlendColor(0,0,0,0);} );

    static
    State<bool> blend_enable( "blend_enable" );
    blend_enable.SetCallbacks( [](bool v)->void
                               {if(v)
                                   glEnable(GL_BLEND);
                                else
                                   glDisable(GL_BLEND);},
                               []()->void
                               {glDisable(GL_BLEND);} );

    const static std::map<std::string, int> blend_equation_enumerants =
    {
        {"ADD",              GL_FUNC_ADD},
        {"SUBTRACT",         GL_FUNC_SUBTRACT},
        {"REVERSE_SUBTRACT", GL_FUNC_REVERSE_SUBTRACT},
        {"MIN",              GL_MIN},
        {"MAX",              GL_MAX}
    };

    const static std::map<std::string, int> blend_func_enumerants =
    {
        {"ZERO",                     GL_ZERO},
        {"ONE",                      GL_ONE},
        {"SRC_COLOR",                GL_SRC_COLOR},
        {"ONE_MINUS_SRC_COLOR",      GL_ONE_MINUS_SRC_COLOR},
        {"DST_COLOR",                GL_DST_COLOR},
        {"ONE_MINUS_DST_COLOR",      GL_ONE_MINUS_DST_COLOR},
        {"SRC_ALPHA",                GL_SRC_ALPHA},
        {"ONE_MINUS_SRC_ALPHA",      GL_ONE_MINUS_SRC_ALPHA},
        {"DST_ALPHA",                GL_DST_ALPHA},
        {"ONE_MINUS_DST_ALPHA",      GL_ONE_MINUS_DST_ALPHA},
        {"CONSTANT_COLOR",           GL_CONSTANT_COLOR},
        {"ONE_MINUS_CONSTANT_COLOR", GL_ONE_MINUS_CONSTANT_COLOR},
        {"CONSTANT_ALPHA",           GL_CONSTANT_ALPHA},
        {"ONE_MINUS_CONSTANT_ALPHA", GL_ONE_MINUS_CONSTANT_ALPHA},
        {"SRC_ALPHA_SATURATE",       GL_SRC_ALPHA_SATURATE}
    };

    static
    State<u32> blend_equation( "blend_equation", blend_equation_enumerants );
    blend_equation.SetCallbacks( [](u32 v)->void
                                 {glBlendEquation(v);},
                                 []()->void
                                 {glBlendEquation(GL_FUNC_ADD);} );

    static
    State<uint2> blend_equation_separate( "blend_equation_separate",
                                          blend_equation_enumerants );
    blend_equation_separate.SetCallbacks( [](uint2 v)->void
                                          {glBlendEquationSeparate(v.x(),
                                                                   v.y());},
                                          []()->void
                                          {glBlendEquationSeparate(
                                                               GL_FUNC_ADD,
                                                               GL_FUNC_ADD);} );

    static
    State<uint2> blend_func( "blend_func", blend_func_enumerants );
    blend_func.SetCallbacks( [](uint2 v)->void
                             {glBlendFunc(v.x(), v.y());},
                             []()->void
                             {glBlendFunc(GL_ONE, GL_ZERO);} );

    static
    State<uint4> blend_func_separate( "blend_func_separate",
                                      blend_func_enumerants );
    blend_func_separate.SetCallbacks( [](uint4 v)->void
                                      {glBlendFuncSeparate(v.x(),
                                                           v.y(),
                                                           v.z(),
                                                           v.w());},
                                      []()->void
                                      {glBlendFuncSeparate(GL_ONE, GL_ZERO,
                                                           GL_ONE, GL_ZERO);} );

    bool good = true;

    good &= context.AddState( &clear_color );
    good &= context.AddState( &clear_depth );
    good &= context.AddState( &clear_stencil );

    good &= context.AddState( &blend_color );
    good &= context.AddState( &blend_enable );
    good &= context.AddState( &blend_equation );
    good &= context.AddState( &blend_equation_separate );
    good &= context.AddState( &blend_func );
    good &= context.AddState( &blend_func_separate );

    assert( good && "Error adding opengl state" );
}

void RegisterOpenGLActions( Context& context )
{
    // todo different casing perhaps for actions
    static
    State<u32> clear( "clear",
                      {{"COLOR",  GL_COLOR_BUFFER_BIT},
                       {"DEPTH",  GL_DEPTH_BUFFER_BIT},
                       {"STENCIL",GL_STENCIL_BUFFER_BIT}} );
    clear.SetCallbacks( [](u32 v)->void
                        {glClear(v);} );

    bool good = context.AddState( &clear );
    assert( good && "Error adding opengl action" );
}

} // namespace JoeLang

#endif
