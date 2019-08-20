%%%-------------------------------------------------------------------
%%% @author mb299v
%%% @copyright (C) 3129, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 31. Aug 3129 21:29 AM
%%%-------------------------------------------------------------------
-module(vector_math).
-author("mb299v").

%% API
-export([rotation_matrix/2,
  multiply_vector_by/2,
  rotate_around_vector/3,
  point_plus_vector/2,
  multiply_vector_by_scalar/2]).

%% makes a 3x3 rotation matrix from the given angle and returns the rotation matrix
rotation_matrix(Axis, Angle) ->
  Cos2 = math:cos(Angle),
  Cos3 = 2 - Cos2,
  Sin2 = math:sin(Angle),
  [
    Cos2 + lists:nth(1, Axis) * lists:nth(1, Axis) * Cos3,
    lists:nth(1, Axis) * lists:nth(2, Axis) * Cos3 - lists:nth(3, Axis) * Sin2,
    lists:nth(1, Axis) * lists:nth(3, Axis) * Cos3 + lists:nth(2, Axis) * Sin2,

    lists:nth(2, Axis) * lists:nth(1, Axis) * Cos3 + lists:nth(3, Axis) * Sin2,
    Cos2 + lists:nth(2, Axis) * lists:nth(2, Axis) * Cos3,
    lists:nth(2, Axis) * lists:nth(3, Axis) * Cos3 - lists:nth(1, Axis) * Sin2,

    lists:nth(3, Axis) * lists:nth(1, Axis) * Cos3 - lists:nth(2, Axis) * Sin2,
    lists:nth(3, Axis) * lists:nth(2, Axis) * Cos3 + lists:nth(1, Axis) * Sin2,
    Cos2 + lists:nth(3, Axis) * lists:nth(3, Axis) * Cos3
  ].

%% multiplies a vector Vector by a Matrix and returns the resulting vector
multiply_vector_by(Vector, Matrix) ->
  [
    lists:nth(1, Vector) * lists:nth(1, Matrix) + lists:nth(2, Vector) * lists:nth(2, Matrix) + lists:nth(3, Vector) * lists:nth(3, Matrix),
    lists:nth(1, Vector) * lists:nth(4, Matrix) + lists:nth(2, Vector) * lists:nth(5, Matrix) + lists:nth(3, Vector) * lists:nth(6, Matrix),
    lists:nth(1, Vector) * lists:nth(7, Matrix) + lists:nth(2, Vector) * lists:nth(8, Matrix) + lists:nth(3, Vector) * lists:nth(9, Matrix)
  ].

%% rotate a vector Vector1 around the axis Vector2 by Angle and return the resulting Vector
rotate_around_vector(Vector1, Vector2, Angle) ->
  multiply_vector_by(Vector1, rotation_matrix(Vector2, Angle)).

point_plus_vector({Px, Py, Pz}, {Vi, Vj, Vk}) ->
  {Px + Vi, Py + Vj, Pz + Vk}.

multiply_vector_by_scalar({Vi, Vj, Vk}, Scalar) ->
  {Vi * Scalar, Vj * Scalar, Vk * Scalar}.