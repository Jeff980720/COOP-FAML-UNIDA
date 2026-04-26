export interface AuthResponse {
    ok: boolean;
    id: string;
    nombre: string;
    username: string;
    rol: string;
    token: string;
    msg?: string; // Por si hay errores
}