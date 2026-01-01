#version 100
// vim:filetype=glsl
precision mediump float;
attribute vec3 vertexPosition;
attribute vec2 vertexTexCoord;
attribute vec4 vertexColor;
varying vec2 fragTexCoord;
varying vec4 fragColor;
// const mat4 u_model = mat4(
// 	0.25, 0., 0., 0.,
// 	0., 0.25, 0., 0.,
// 	0., 0., 1., 0.,
// 	0., 0., 0., 1.
// );
uniform mat4 u_model;
uniform mat4 mvp;
void main()
{
	fragTexCoord = vertexTexCoord;
	fragColor = vertexColor;
	gl_Position = mvp * u_model * vec4(vertexPosition, 1.0);
}
